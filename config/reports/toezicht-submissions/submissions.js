import {batchedQuery} from '../../helpers.js';
import {generateReportFromQueryResult} from '../util/report-helpers';


export default async function(date = new Date()) {
  const metadata = {
    title: `Toezicht module: Meldingen`,
    description: `Bevat alle meldingen gemaakt in de Toezicht module van het jaar ${date.getFullYear()} tot nu.`,
    filePrefix: `toezicht-meldingen`,
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
 * Query that **attempts** to extract all UNIQUE submissions made for the Toezicht module.
 *
 * The query exists out of three nested sub-queries (from most outer to inner):
 * - Ensure we only get the latest session-started-date and date-publication, to counteract doubles
 * - Little hack to counteract rare instance off double form-data (this works as the doubles that currently exist are exact copies)
 * - Get me all Submissions, within the Toezicht module, sent after given date (to optimize for OPTIONAL usage)
 *
 * Known ambiguities (resulting in more lines per submission):
 *  - some have multiple creators
 *  - some have multiple decision-types
 *  - some have multiple bestuursorganen-in-tijd
 *  - some have multiple submitted-resources
 *
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
SELECT DISTINCT 
(?submission AS ?melding) 
(?labelBestuurseenheidClassification AS ?typeBestuur) 
?labelBestuurseenheid
?bestuurseenheid
(?labelBestuursorgaanClassificatie AS ?typeBestuursorgaan)
?bestuursorgaanInTijd
?aangemaaktDoor
?datumZitting
?datumPublicatie
?datumVerstuurd
(?labelDecisionType AS ?typeDossier)
(?labelRegulationType AS ?typeReglementOfVerordening)
(?labelTaxType AS ?typeBelasting)
(?submittedResource AS ?link)
WHERE {
  {
    SELECT ?toezichtGraph ?submission ?datumVerstuurd ?formData (MAX(?sessionStartedAtTime) AS ?datumZitting) (MAX(?datePublication) AS ?datumPublicatie)
    WHERE {
      {
        SELECT ?toezichtGraph ?submission ?datumVerstuurd (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            SELECT DISTINCT ?toezichtGraph ?submission ?datumVerstuurd
            WHERE {
              GRAPH ?toezichtGraph {
                ?submission a meb:Submission;
                            nmo:sentDate ?datumVerstuurd.
              }
              FILTER ( ?datumVerstuurd >= "${date.getFullYear()}-01-01T00:00:00.000Z"^^xsd:dateTime )
              FILTER(REGEX(str(?toezichtGraph), """http://mu.semte.ch/graphs/organizations/.*/LoketLB-toezichtGebruiker"""))
            }
          }
          GRAPH ?toezichtGraph {
            ?submission prov:generated ?generatedDataSubmission.
          }
        }
        GROUP BY ?toezichtGraph ?submission ?datumVerstuurd
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
    }
    GROUP BY ?toezichtGraph ?submission ?datumVerstuurd ?formData 
  }
  GRAPH ?toezichtGraph {
    ?submission
      pav:createdBy ?bestuurseenheid.
  }
  GRAPH <http://mu.semte.ch/graphs/public> {
    ?bestuurseenheid skos:prefLabel ?labelBestuurseenheid;
                     besluit:classificatie/skos:prefLabel ?labelBestuurseenheidClassification.
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
      ?submission dct:creator ?creator.
    }
    GRAPH ?orgGraph {
      ?creator 
        foaf:firstName ?firstNameCreator;
        foaf:familyName ?familyNameCreator.
    }
    BIND(CONCAT(?firstNameCreator, " ", ?familyNameCreator) as ?aangemaaktDoor)
  }  
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData ext:decisionType ?decisionType .
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?decisionType skos:prefLabel ?labelDecisionType .
    }
  }
  OPTIONAL {
    GRAPH ?toezichtGraph {
      ?formData ext:regulationType ?regulationType .
    }
    GRAPH <http://mu.semte.ch/graphs/public> {
      ?regulationType skos:prefLabel ?labelRegulationType.
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
      ?submission dct:subject ?submittedResource
    }
  }
}
ORDER BY ?submission`;