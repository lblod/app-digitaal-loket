import {generateReportFromData, batchedQuery} from '../../helpers.js';

const metadata = {
  title: 'Toezicht Module: Geuploade bestanden',
  description: 'TODO',
  filePrefix: `toezicht-geuploade-bestanden`,
};

export default async function() {
  let bindings = [];
  try {
    const queryString = generateMotherOfAllQueries();
    const {results} = await batchedQuery(queryString);
    bindings = results.bindings;
  } catch (e) {
    throw `Something unexpected went wrong when executing the batched-query for [${metadata.title}]`;
  }
  if (bindings.length === 0) {
    console.warn('[WARN] No uploaded files could be found, no report will be generated.');
  } else {
    const data = bindings.map(row => {
      const getSafeValue = (entry, property) => entry[property] ? `\"${entry[property].value}\"` : null;
      return {
        submission: getSafeValue(row, 'submission'),
        fileName: getSafeValue(row, 'fileName'),
        url: getSafeValue(row, 'url')
      };
    });
    await generateReportFromData(data, Object.keys(data[0]), metadata);
  }
}

// TODO document this ...
const generateMotherOfAllQueries = (date = new Date()) => `PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX pr: <http://purl.org/ontology/prv/core#>
PREFIX pro: <http://purl.org/hpi/patchr#>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
SELECT DISTINCT ?submission ?fileName ?url
WHERE {
  {
    SELECT ?submission ?fileName
    WHERE {
      {
        # Little hack to counter act double form-data (this works as the doubles that currently exsist are exact copies)
        SELECT ?toezichtGraph ?submission (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            # Get me all Submissions, within the the Toezicht module, sent after ...
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
        # Little hack to counter act double form-data (this works as the doubles that currently exsist are exact copies)
        SELECT ?toezichtGraph ?submission (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            # Get me all Submissions, within the the Toezicht module, sent after ...
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