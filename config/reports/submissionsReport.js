import {generateReportFromData, batchedQuery} from '../helpers.js';

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'submissions',
  execute: async () => {
    const reportData = {
      title: 'List of the submissions of the Toezicht part of Loket',
      description: 'Submissions with their date, the type of submission, the unit, the type of unit and the date of the meeting.',
      filePrefix: 'submissions'
    };

    const dateFrom = "2020-09-01T00:00:00.000Z";

    console.log('Generate Submissions Report');

    // Get the submissions and uploaded files
    const queryStringPart1 = `
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>

      SELECT ?s ?file WHERE { 
        GRAPH ?g { 
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            prov:generated ?formData .
          ?formData dct:hasPart ?attachmentFile .
        }
        GRAPH ?h {
          ?attachmentFile a nfo:FileDataObject ;
            nfo:fileName ?file .
        }
        FILTER ( ?verstuurd >= "${dateFrom}"^^xsd:dateTime )
      }
    `;

    const queryResponsePart1 = await batchedQuery(queryStringPart1);
    const dataToEnrich = queryResponsePart1.results.bindings.map((row) => ({
        s: getSafeValue(row, 's'),
        file: getSafeValue(row, 'file')
    }));

    // Group the files in a field
    groupFilesBySubmission(dataToEnrich);

    const dataPart1 = dataToEnrich.reduce( (acc, row) => {
      acc[row.s] = {
        s: row.s,
        files: row.files
      };
      return acc;
    }, {});

    // Get the submissions with all required fields and combine it with the files
    const queryStringPart2 = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX pav: <http://purl.org/pav/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
      PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
      PREFIX eli: <http://data.europa.eu/eli/ontology#>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>

      SELECT ?s ?verstuurd ?typeDossier ?typeReglementOfVerordening ?soortBelasting ?bestuurseenheidLabel ?typeBestuur ?datumZitting ?statusLabel ?angemaaktDoor ?gewijzigdDoor ?link ?bot ?bestuursorgaan ?bestuursorgaanLabel ?bestuursorgaanClassificatieLabel
       WHERE { 
        GRAPH ?g { 
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            pav:createdBy ?bestuurseenheid ;
            adms:status ?status ;
            prov:generated ?formData .
          ?formData ext:decisionType ?type ;
            ext:sessionStartedAtTime ?datumZitting .

          FILTER ( ?verstuurd >= "${dateFrom}"^^xsd:dateTime )
        }
        GRAPH ?h {
          ?type skos:prefLabel ?typeDossier .
          ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel ;
            besluit:classificatie/skos:prefLabel ?typeBestuur .
          ?status skos:prefLabel ?statusLabel .
        }

        OPTIONAL {
          GRAPH ?g {
            ?s dct:creator ?createdBy .
          }
          GRAPH ?i {
            ?createdBy foaf:firstName ?angemaaktDoorFirstName ;
              foaf:familyName ?angemaaktDoorLastName .
          }
          BIND(CONCAT(?angemaaktDoorFirstName, " ", ?angemaaktDoorLastName) as ?angemaaktDoor)
        }
        OPTIONAL {
          GRAPH ?g {
            ?s ext:lastModifiedBy ?modifiedBy .
          }
          GRAPH ?i {
            ?modifiedBy foaf:firstName ?gewijzigdDoorFirstName ;
              foaf:familyName ?gewijzigdDoorLastName .
          }
          BIND(CONCAT(?gewijzigdDoorFirstName, " ", ?gewijzigdDoorLastName) as ?gewijzigdDoor)
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData ext:regulationType ?regulationType .
          }
          GRAPH ?h {
            ?regulationType skos:prefLabel ?typeReglementOfVerordening .
          }
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData ext:taxType ?taxType .
          }
          GRAPH ?h {
            ?taxType skos:prefLabel ?soortBelasting .
          }
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData dct:hasPart ?attachmentLink .
          }
          GRAPH ?h {
            ?attachmentLink a nfo:RemoteDataObject ;
              nie:url ?link .
          }
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData eli:passed_by ?bot .
          }
          GRAPH <http://mu.semte.ch/graphs/public> {
           ?bot mandaat:isTijdspecialisatieVan ?bestuursorgaan.
           ?bestuursorgaan skos:prefLabel ?bestuursorgaanLabel.
           ?bestuursorgaan besluit:classificatie/skos:prefLabel ?bestuursorgaanClassificatieLabel.
          }
        }

      }
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        verstuurd: getSafeValue(row, 'verstuurd'),
        typeDossier: getSafeValue(row, 'typeDossier'),
        typeReglementOfVerordening: getSafeValue(row, 'typeReglementOfVerordening'),
        soortBelasting: getSafeValue(row, 'soortBelasting'),
        bestuurseenheidLabel: getSafeValue(row, 'bestuurseenheidLabel'),
        bestuursorgaanInTijd: getSafeValue(row, 'bot'),
        bestuursorgaan: getSafeValue(row, 'bestuursorgaan'),
        bestuursorgaanLabel: getSafeValue(row, 'bestuursorgaanLabel'),
        bestuursorgaanClassificatie: getSafeValue(row, 'bestuursorgaanClassificatieLabel'),
        typeBestuur: getSafeValue(row, 'typeBestuur'),
        angemaaktDoor: getSafeValue(row, 'angemaaktDoor'),
        gewijzigdDoor: getSafeValue(row, 'gewijzigdDoor'),
        statusLabel: getSafeValue(row, 'statusLabel'),
        datumZitting: getSafeValue(row, 'datumZitting'),
        link: getSafeValue(row, 'link')      };
      acc[getSafeValue(row, 's')] = Object.assign(dataPart, dataPart1[getSafeValue(row, 's')]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart2), [
      'verstuurd',
      'typeDossier',
      'typeReglementOfVerordening',
      'soortBelasting',
      'bestuurseenheidLabel',
      'bestuursorgaanInTijd',
      'bestuursorgaan',
      'bestuursorgaanLabel',
      'bestuursorgaanClassificatie',
      'typeBestuur',
      'angemaaktDoor',
      'gewijzigdDoor',
      'statusLabel',
      'datumZitting',
      'link',
      'files'
    ], reportData);
  }
};

function groupFilesBySubmission(data) {
  data.forEach(o => {
    const filesFromSameSubmission = data.filter(submission => submission.s == o.s);
    if (filesFromSameSubmission && filesFromSameSubmission.length) {
      o.files = filesFromSameSubmission.map(f => f.file).join(",");
    }
  })
}

function getSafeValue(entry, property){
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
