import {generateReportFromData, batchedQuery} from '../../helpers.js';

const metadata = {
  title: 'Toezicht Module: Meldingen',
  description: 'List of all "Meldingen" made in the Toezicht module of Loket, for THIS year',
  filePrefix: `toezicht-meldingen`,
};

export default async function() {
  let bindings = [];
  try {
    const queryString = generateMotherOfAllQueries();
    const {results} = await batchedQuery(queryString, 10000);
    bindings = results.bindings;
  } catch (e) {
    throw `Something unexpected went wrong when executing the batched-query for [${metadata.title}]`;
  }
  if (bindings.length === 0) {
    console.warn('[WARN] No meldingen could be found, no report will be generated.');
  } else {
    const data = bindings.map(row => {
      const getSafeValue = (entry, property) => entry[property] ? `\"${entry[property].value}\"` : null;
      return {
        submission: getSafeValue(row, 'submission'),
        typeBestuur: getSafeValue(row, 'labelBestuurseenheidClassification'),
        bestuurseenheid: getSafeValue(row, 'labelBestuurseenheid'),
        typeBestuursorgaan: getSafeValue(row,
            'labelBestuursorgaanClassificatie'),
        bestuursorgaanInTijd: getSafeValue(row, 'bestuursorgaanInTijd'),
        aangemaaktDoor: getSafeValue(row, 'aangemaaktDoor'),
        datumZitting: getSafeValue(row, 'datumZitting'),
        datumPublicatie: getSafeValue(row, 'datumPublicatie'),
        datumVerstuurd: getSafeValue(row, 'datumVerstuurd'),
        typeDossier: getSafeValue(row, 'labelDecisionType'),
        typeReglementOfVerordening: getSafeValue(row, 'labelRegulationType'),
        typeBelasting: getSafeValue(row, 'labelTaxType'),
        meldingURL: getSafeValue(row, 'submittedResource')
      };
    });
    await generateReportFromData(data, Object.keys(data[0]), metadata);
  }
}

// TODO document this ...
const generateMotherOfAllQueries = (date = new Date()) => `PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
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
SELECT 
?submission 
?labelBestuurseenheidClassification 
?labelBestuurseenheid
?labelBestuursorgaanClassificatie
?bestuursorgaanInTijd
?aangemaaktDoor
?datumZitting
?datumPublicatie
?datumVerstuurd
?labelDecisionType
?labelRegulationType
?labelTaxType
?submittedResource
WHERE {
  {
    # Ensure we only get the latest session-started-date and date-publication, to counter act doubles 
    SELECT ?toezichtGraph ?submission ?datumVerstuurd ?formData (MAX(?sessionStartedAtTime) AS ?datumZitting) (MAX(?datePublication) AS ?datumPublicatie)
    WHERE {
      {
        # Little hack to counter act double form-data (this works as the doubles that currently exsist are exact copies)
        SELECT ?toezichtGraph ?submission ?datumVerstuurd (MIN(?generatedDataSubmission) AS ?formData)
        WHERE {
          {
            # Get me all Submissions, within the the Toezicht module, sent after ...
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
  # OPTIONAL VALUES: values that could be empty
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