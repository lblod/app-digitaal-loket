import {batchedQuery} from '../../helpers.js';
import {generateReportFromQueryResult} from '../util/report-helpers';

export default async function(date = new Date()) {
  const metadata = {
    title: `Toezicht module: Geüploade bestanden`,
    description: `Bevat alle bestanden geüpload voor meldingen in de Toezicht module van het jaar ${date.getFullYear()} tot nu.`,
    filePrefix: `toezicht-geuploade-bestanden`,
  };
  try {
    const queryString = generateMotherOfAllQueries(date);
    const result = await batchedQuery(queryString, 10000)
    await generateReportFromQueryResult(result, metadata);
  } catch (e) {
    throw `Something unexpected went wrong when executing report for [${metadata.title}]`;
  }
}

/**
 * Query that **attempts** to extract all uploaded files for submissions made for the Toezicht module.
 *
 * The query exists out of two nested sub-queries (from most outer to inner):
 * - Little hack to counteract rare instance off double form-data (this works as the doubles that currently exist are exact copies)
 * - Get me all Submissions, within the Toezicht module, sent after given date (to optimize for OPTIONAL usage)
 **/
const generateMotherOfAllQueries = (date = new Date()) => `
PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX pr: <http://purl.org/ontology/prv/core#>
PREFIX pro: <http://purl.org/hpi/patchr#>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
SELECT DISTINCT (?submission AS ?melding) (?fileName AS ?bestandsnaam) (?url AS ?link)
WHERE {
  {
    SELECT ?submission ?fileName
    WHERE {
      {
        SELECT ?toezichtGraph ?submission (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            SELECT DISTINCT ?toezichtGraph ?submission
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
        GROUP BY ?toezichtGraph ?submission
      }
      GRAPH ?toezichtGraph {
        ?formData dct:hasPart ?fileAttachement.
      }
      GRAPH ?fileGraph {
        ?fileAttachement
          a nfo:FileDataObject;
          nfo:fileName ?fileName.
      }
    }
  }
  UNION
  {
    SELECT ?submission ?fileName ?url
    WHERE {
      {
        SELECT ?toezichtGraph ?submission (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            SELECT DISTINCT ?toezichtGraph ?submission
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
        GROUP BY ?toezichtGraph ?submission
      }
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
ORDER BY ?submission`;