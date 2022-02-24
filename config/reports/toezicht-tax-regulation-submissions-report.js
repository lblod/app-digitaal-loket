import {batchedQuery} from '../helpers.js';
import {generateReportFromQueryResult} from './util/report-helpers';

const metadata = {
  title: 'Toezicht module: Meldingen voor de MAR-codes 7300, 7304 en 7305',
  description: 'Bevat alle belastingreglement meldingen ingediend in de Toezicht module met als gekozen MAR-code: 7300, 7304 of 7305',
  filePrefix: `toezicht-belastingreglement-meldingen`,
};

const MAR_CODES = [
  // MAR 7300
  'http://lblod.data.gift/concepts/26c19fe6b53e2e759a0b9467ce33ef37fc268dd9467cfba91381214549a01d19',
  // MAR 7304
  'http://lblod.data.gift/concepts/fad34acdd5a6d17b59d58678542b65e9c364d2c84bf49fea8611ecc2d6ce3411',
  // MAR 7305
  'http://lblod.data.gift/concepts/514defe410ee9b1b6e01433fbbca7ce16c48d24f0785c1f3c2cb86c44af3e50a',
];

export default {
  cronPattern: '0 0 23 * * *',
  name: 'toezicht-tax-regulation-submissions',
  execute: async () => {
    const startTime = new Date();
    console.log(
        `[INFO] Starting with reports for [${metadata.title}] @ ${startTime.toISOString()}`);
    try {
      const queryString = generateMotherOfAllQueries(MAR_CODES);
      const result = await batchedQuery(queryString);
      await generateReportFromQueryResult(result, metadata);
    } catch (e) {
      throw `Something unexpected went wrong when executing report for [${metadata.title}]`;
    }
    console.log(
        `[INFO] Finished reports for [${metadata.title}] @ ${new Date().toISOString()}. ` +
        `Time elapsed: ${Math.abs(new Date() - startTime)}ms`,
    );
  },
};

/**
 * Query that **attempts** to extract all tax-regulation submissions made in the Toezicht module for the given MAR-codes.
 *
 * The query exists out of two nested sub-queries (from most outer to inner):
 * - Ensure we only get the latest: session-started-date, date-publication, date-in-force and date-no-longer-in-force to counteract doubles
 * - Get me all tax-regulation Submissions for the given MAR-code, within the Toezicht module, that have a sent-date
 *
 * Known array values (resulting in more lines per submission):
 *  - links-to-documents
 *  - uploaded files
 *  - tax-rate-amount (opcentiem-bedrag)
 *
 * Known ambiguities (resulting in more lines per submission):
 *  - some could have multiple bestuursorganen-in-tijd
 *  - some could have multiple submitted-resources
 **/
const generateMotherOfAllQueries = (codes) => `
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
PREFIX lblodBesluit: <http://lblod.data.gift/vocabularies/besluit/>
SELECT DISTINCT 
(?submission AS ?melding) 
(?labelBestuurseenheidClassification AS ?typeBestuur) 
(?labelBestuurseenheid AS ?bestuurseenheid)
(?labelBestuursorgaanClassificatie AS ?typeBestuursorgaan)
?datumZitting
?datumPublicatie
?datumVerstuurd
(?labelTaxType AS ?typeBelasting)
(?hasAdditionalTaxRate AS ?sprakeVanDifferentiatie)
(?taxRateAmount AS ?opcentiemBedrag)
(?notationChartOfAccount AS ?MARCode)
?datumGeldtVanaf
?datumGeldtTot
(?fileName AS ?bestandsnaam)
(?url AS ?linkNaarDocument)
(?submittedResource AS ?linkMeldingsplichtige)
WHERE {
  {
    SELECT 
    ?toezichtGraph 
    ?submission 
    ?datumVerstuurd 
    ?formData 
    ?chartOfAccount
    (MAX(?sessionStartedAtTime) AS ?datumZitting) 
    (MAX(?datePublication) AS ?datumPublicatie)
    (MAX(?dateInForce) AS ?datumGeldtVanaf)
    (MAX(?dateNoLongerInForce) AS ?datumGeldtTot)
    WHERE {
      {
        SELECT DISTINCT 
        ?toezichtGraph 
        ?submission 
        ?datumVerstuurd 
        (?generatedDataSubmission AS ?formData)
        ?chartOfAccount
        WHERE {
          VALUES ?chartOfAccount {
            ${codes.map(code => `<${code}>`).join('\n            ')}
          }
          GRAPH ?toezichtGraph {
            ?submission a meb:Submission;
                        nmo:sentDate ?datumVerstuurd;
                        prov:generated ?generatedDataSubmission.
            ?generatedDataSubmission 
              ext:decisionType <https://data.vlaanderen.be/id/concept/BesluitType/67378dd0-5413-474b-8996-d992ef81637a>;
              ext:regulationType <https://data.vlaanderen.be/id/concept/BesluitType/efa4ec5a-b006-453f-985f-f986ebae11bc>;
              lblodBesluit:chartOfAccount ?chartOfAccount.
          }
          FILTER(REGEX(str(?toezichtGraph), """http://mu.semte.ch/graphs/organizations/.*/LoketLB-toezichtGebruiker"""))
        }
      }
      OPTIONAL {
        GRAPH ?toezichtGraph {
          ?formData ext:sessionStartedAtTime ?sessionStartedAtTime.
        }
      }
      OPTIONAL {
        GRAPH ?toezichtGraph {
          ?formData eli:date_publication ?datePublication.
        }
      }
      OPTIONAL {
        GRAPH ?toezichtGraph {
          ?formData eli:first_date_entry_in_force ?dateInForce.
        }
      }
      OPTIONAL {
        GRAPH ?toezichtGraph {
          ?formData eli:date_no_longer_in_force ?dateNoLongerInForce.
        }
      }
    }
    GROUP BY ?toezichtGraph ?submission ?datumVerstuurd ?formData ?chartOfAccount
  }
  GRAPH ?toezichtGraph {
    ?submission
      pav:createdBy ?bestuurseenheid.
  }
  GRAPH <http://mu.semte.ch/graphs/public> {
    ?bestuurseenheid skos:prefLabel ?labelBestuurseenheid;
                     besluit:classificatie/skos:prefLabel ?labelBestuurseenheidClassification.
    ?chartOfAccount skos:notation ?notationChartOfAccount.
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData eli:passed_by ?bestuursorgaanInTijd .
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?bestuursorgaanInTijd mandaat:isTijdspecialisatieVan ?bestuursorgaan.
      ?bestuursorgaan besluit:classificatie/skos:prefLabel ?labelBestuursorgaanClassificatie.
    }
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData ext:taxType ?taxType .
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?taxType skos:prefLabel ?labelTaxType .
    }
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData lblodBesluit:hasAdditionalTaxRate ?hasAdditionalTaxRate .
    }
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData ext:taxRateAmount ?taxRateAmount 
    }
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?submission dct:subject ?submittedResource
    }
  }
  {
    OPTIONAL {
      GRAPH ?toezichtGraph {
        ?formData dct:hasPart ?fileAttachement.
      }
      GRAPH ?fileGraph {
        ?fileAttachement
          a nfo:FileDataObject;
          nfo:fileName ?fileName.
      }
    }
  } UNION
  {
    OPTIONAL {
      GRAPH ?toezichtGraph {
        ?formData dct:hasPart ?remoteAttachement.
      }
      GRAPH ?fileGraph {
        ?remoteAttachement 
          a nfo:RemoteDataObject;
          nie:url ?url.
        ?attachementFile 
          a nfo:FileDataObject;
          nfo:fileName ?fileName;
          nie:dataSource ?remoteAttachement.
      }
    }
  }
}
ORDER BY DESC(?datumVerstuurd) ?submission`;