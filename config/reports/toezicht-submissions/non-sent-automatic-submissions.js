import {batchedQuery} from '../../helpers.js';
import {generateReportFromQueryResult} from '../util/report-helpers';

const HANGING_SUBMISSION_STATUSES = [
  'http://lblod.data.gift/concepts/79a52da4-f491-4e2f-9374-89a13cde8ecd', // concept
  'http://lblod.data.gift/concepts/f6330856-e261-430f-b949-8e510d20d0ff', // inzendbaar
];

const DETAIL_BATCH_SIZE = 10;

export default async function(date = new Date()) {
  const metadata = {
    title: `Niet verstuurde automatische meldingen (a.k.a hangende meldingen)`,
    description: `
      Overzicht van automatische meldingen die zich in status [concept] of [inzendbaar] bevinden (hangende meldingen) van het jaar ${date.getFullYear()} tot nu.
      We hebben pas informatie in de databank van zodra een melding succesvol geregistreerd werd.
      M.a.w: o.a meldingen waarbij de authenticatie gefaald is, gaan hier niet verschijnen.
      De informatie is beperkt. Het doel van dit rapport is inschatten of er iets structureels misloopt.
      Voor meer gedetailleerde informatie kan u steeds aanloggen als het bestuur in kwestie op het jobs-dashboard of de controle-omgeving.
      De URL naar het dashboard job en controle zijn meegenomen. Steeds aanloggen als het bestuur! Werkt enkel op productie.
      Noteer: een update werd 01/07/2026 aan het rapport gedaan, u gaat mogelijks meer cases zien. Zie DL-7461 voor meer info.
    `.replace(/[\n\r]+/g, ' '),
    filePrefix: `niet-verstuurde-automatische-meldingen`
  };
  try {
    const candidates = await fetchHangingSubmissions(date);
    if (!candidates.length) {
      console.warn('[WARN] nothing to report on ...');
      return;
    }
    const result = await fetchDetailsForSubmissions(candidates);
    await generateReportFromQueryResult(result, metadata);
  } catch (e) {
    throw `Something unexpected went wrong when executing report for [${metadata.title}]`;
  }
}

/**
 * Step 1: cheaply find the candidate (submission, job) pairs.
 * Only joins Job -> Submission -> adms:status, using VALUES on the hanging
 * statuses instead of a FILTER NOT EXISTS on "verzonden". Bounded by the
 * job-created date window. Ordered by ?submission for consistent pagination.
 **/
async function fetchHangingSubmissions(date) {
  const result = await batchedQuery(generateCandidateQuery(date), 10000);
  return result?.results?.bindings ?? [];
}

const generateCandidateQuery = (date = new Date()) => `
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX adms: <http://www.w3.org/ns/adms#>

SELECT DISTINCT ?submission ?job
WHERE {
  ?job prov:generated ?submission;
       <http://redpencil.data.gift/vocabularies/tasks/operation> <http://lblod.data.gift/id/jobs/concept/JobOperation/automaticSubmissionFlow>;
       dct:created ?jobCreated.
  ?submission adms:status ?submissionStatus.
  VALUES ?submissionStatus {
    ${HANGING_SUBMISSION_STATUSES.map((s) => `<${s}>`).join('\n    ')}
  }
  FILTER ( ?jobCreated >= "${date.getFullYear()}-01-01T00:00:00.000Z"^^xsd:dateTime )
}
ORDER BY ?submission`;

/**
 * Step 2: for each candidate (submission, job) pair, fetch the report columns.
 * Driven by VALUES (?submission ?job) so every UNION branch below is a bounded
 * lookup. Each attribute is fetched in its own UNION branch (one sparse row per
 * matching branch), so a submission no longer drops out when bestuurseenheid /
 * vendor / subject / url / labels are missing in the databank. Second-hop label
 * triples stay grouped with their subject-binding triple to avoid unbound scans.
 * The sparse rows are merged back into one row per (submission, job) in JS,
 * preserving the {value, ...} binding shape so generateReportFromQueryResult
 * can consume the result unchanged.
 **/
async function fetchDetailsForSubmissions(candidateBindings) {
  const pairs = candidateBindings.map((b) => [b.submission.value, b.job.value]);
  const merged = new Map();
  let head = {vars: []};
  for (const chunk of chunkArray(pairs, DETAIL_BATCH_SIZE)) {
    const result = await batchedQuery(generateDetailQuery(chunk), 10000);
    if (result?.head?.vars?.length) {
      head = result.head;
    }
    for (const row of result?.results?.bindings ?? []) {
      const key = `${row.submission?.value}|${row.job?.value}`;
      const target = merged.get(key) ?? {};
      for (const v of head.vars) {
        if (row[v] && !target[v]) target[v] = row[v];
      }
      merged.set(key, target);
    }
  }
  return {head, results: {bindings: [...merged.values()]}};
}

const generateDetailQuery = (pairs) => `
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX adms: <http://www.w3.org/ns/adms#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX pav: <http://purl.org/pav/>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

SELECT DISTINCT
  ?bestuurseenheidLabel
  ?bestuurseenheidClassLabel
  ?submissionStatusLabel
  ?jobCreatedShort
  ?jobStatusShort
  ?dashboardUrl
  ?job
  ?jobCreated
  ?jobStatus
  ?submission
  ?subject
  ?url
  ?vendor
  ?vendorLabel
  ?submissionStatus
  ?bestuurseenheid
  ?classificatie
WHERE {
  VALUES (?submission ?job) {
    ${pairs.map(([s, j]) => `(<${s}> <${j}>)`).join('\n    ')}
  }
  {
    ?job a <http://vocab.deri.ie/cogs#Job>;
         mu:uuid ?jobUuid.
    BIND(CONCAT("https://dashboard.loket.lokaalbestuur.vlaanderen.be/jobs/", ?jobUuid) as ?dashboardUrl)
  }
  UNION {
    ?job a <http://vocab.deri.ie/cogs#Job>;
         adms:status ?jobStatus.
    BIND(STRAFTER(STR(?jobStatus), "http://redpencil.data.gift/id/concept/JobStatus/") as ?jobStatusShort)
  }
  UNION {
    ?job a <http://vocab.deri.ie/cogs#Job>;
         dct:created ?jobCreated.
    BIND(CONCAT(STR(YEAR(?jobCreated)), '-', STR(MONTH(?jobCreated)), '-', STR(DAY(?jobCreated))) as ?jobCreatedShort)
  }
  UNION {
    ?submission a meb:Submission;
                adms:status ?submissionStatus.
  }
  UNION {
    ?submission a meb:Submission;
                adms:status ?submissionStatus.
    ?submissionStatus skos:prefLabel ?submissionStatusLabel.
  }
  UNION {
    ?submission a meb:Submission;
                dct:subject ?subject.
  }
  UNION {
    ?submission a meb:Submission;
                prov:atLocation ?url.
  }
  UNION {
    ?submission a meb:Submission;
                pav:providedBy ?vendor.
    ?vendor foaf:name ?vendorLabel.
  }
  UNION {
    ?submission a meb:Submission;
                pav:createdBy ?bestuurseenheid.
    ?bestuurseenheid a besluit:Bestuurseenheid;
      besluit:classificatie ?classificatie;
      skos:prefLabel ?bestuurseenheidLabel.
    ?classificatie skos:prefLabel ?bestuurseenheidClassLabel.
  }
}
ORDER BY ?submission`;

function chunkArray(array, size) {
  const chunks = [];
  for (let i = 0; i < array.length; i += size) {
    chunks.push(array.slice(i, i + size));
  }
  return chunks;
}
