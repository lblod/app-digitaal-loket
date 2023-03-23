import {batchedQuery} from '../../helpers.js';
import {generateReportFromQueryResult} from '../util/report-helpers';


export default async function(date = new Date()) {
  const metadata = {
    title: `Niet verstuurde automatische meldingen (a.k.a hangende meldingen)`,
    description: `
      Overzicht van automatische meldingen die niet op status [verzonden] staan van het jaar ${date.getFullYear()} tot nu.
      We hebben pas informatie in de databank van zodra een melding succesvol gergistreerd werd.
      M.a.w: o.a meldingen waarbij de authenticatie gefaald is, gaan hier niet verschijnen.
      De informatie is beperkt. Het doel van dit rapport is inschatten of er iets structureels misloopt.
      Voor meer gedetailleerde informatie kan u steeds aanloggen als het bestuur in kwestie op het jobs-dashboard of de controle-omgeving.
      De URL naar het dashboard job en controle zijn meegenomen. Steeds aanloggen als het bestuur! Werkt enkel op productie.
    `.replace(/[\n\r]+/g, ' '), //Note: we can't access sparqlUtils here.
    filePrefix: `niet-verstuurde-automatische-meldingen`
  };
  try {
    const queryString = generateMotherOfAllQueries(date);
    const result = await batchedQuery(queryString, 10000);
    await generateReportFromQueryResult(result, metadata);
  } catch (e) {
    throw `Something unexpected went wrong when executing report for [${metadata.title}]`;
  }
}

/**
 * cf. supra what the query attempts.
 * Note: make sure you keep ordering on ?job to allow consistent pagination.
 **/
const generateMotherOfAllQueries = (date = new Date()) => `
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX pav: <http://purl.org/pav/>
PREFIX adms: <http://www.w3.org/ns/adms#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX fo: <http://purl.org/ontology/fo/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
PREFIX eli: <http://data.europa.eu/eli/ontology#>
PREFIX ma: <http://www.w3.org/ns/ma-ont#>
PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

SELECT DISTINCT
?bestuurseenheidLabel
?bestuurseenheidClassLabel
?submissionStatusLabel
?jobCreatedShort
?jobStatusShort
?dashboardUrl
?controleUrl
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
  ?job a <http://vocab.deri.ie/cogs#Job>;
      <http://www.w3.org/ns/prov#generated> ?submission;
      mu:uuid ?jobUuid;
      adms:status ?jobStatus;
      <http://redpencil.data.gift/vocabularies/tasks/operation> <http://lblod.data.gift/id/jobs/concept/JobOperation/automaticSubmissionFlow>;
      <http://purl.org/dc/terms/created> ?jobCreated.

  ?submission a <http://rdf.myexperiment.org/ontologies/base/Submission>;
    mu:uuid ?uuid;
    <http://purl.org/dc/terms/subject> ?subject;
    <http://www.w3.org/ns/prov#atLocation> ?url;
    <http://purl.org/pav/providedBy> ?vendor;
    <http://www.w3.org/ns/adms#status> ?submissionStatus;
    <http://purl.org/pav/createdBy> ?bestuurseenheid.

   FILTER NOT EXISTS {
    ?submission <http://www.w3.org/ns/adms#status> <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c>.
   }

   ?bestuurseenheid a besluit:Bestuurseenheid;
    besluit:classificatie ?classificatie;
    skos:prefLabel ?bestuurseenheidLabel.

   ?classificatie skos:prefLabel ?bestuurseenheidClassLabel.

   ?vendor foaf:name ?vendorLabel.
   ?submissionStatus skos:prefLabel ?submissionStatusLabel.

  BIND(CONCAT("https://controle.loket.lblod.info/supervision/submissions/", ?uuid) as ?controleUrl)
  BIND(CONCAT("https://dashboard.prod.lblod.info/jobs/", ?jobUuid, "/index") as ?dashboardUrl)
  BIND(CONCAT(STR(YEAR(?jobCreated)), '-', STR(MONTH(?jobCreated)), '-', STR(DAY(?jobCreated))) as ?jobCreatedShort)
  BIND(STRAFTER(STR(?jobStatus), "http://redpencil.data.gift/id/concept/JobStatus/") as ?jobStatusShort)

  FILTER ( ?jobCreated >= "${date.getFullYear()}-01-01T00:00:00.000Z"^^xsd:dateTime )
}
ORDER BY ?job`;
