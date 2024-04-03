import {generateReportFromData, batchedQuery} from '../helpers.js';

/**
 * [DEPRECATED] go see {@link toezicht-module-report}
 */
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
        s: row?.s?.value,
        file: row?.file?.value
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

      SELECT ?s
        ?verstuurd
        ?typeDossier
        ?typeReglementOfVerordening
        ?soortBelasting
        ?bestuurseenheidLabel
        ?typeBestuur
        ?datumZitting
        ?statusLabel
        ?angemaaktDoor
        ?gewijzigdDoor
        ?link
        ?bot
        ?bestuursorgaan
        ?bestuursorgaanLabel
        ?bestuursorgaanClassificatieLabel
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

        FILTER(REGEX(str(?g), """http://mu.semte.ch/graphs/organizations/.*/LoketLB-toezichtGebruiker"""))

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
        verstuurd: row?.verstuurd?.value,
        typeDossier: row?.typeDossier?.value,
        typeReglementOfVerordening: row?.typeReglementOfVerordening?.value,
        soortBelasting: row?.soortBelasting?.value,
        bestuurseenheidLabel: row?.bestuurseenheidLabel?.value,
        bestuursorgaanInTijd: row?.bot?.value,
        bestuursorgaan: row?.bestuursorgaan?.value,
        bestuursorgaanLabel: row?.bestuursorgaanLabel?.value,
        bestuursorgaanClassificatie: row?.bestuursorgaanClassificatieLabel?.value,
        typeBestuur: row?.typeBestuur?.value,
        angemaaktDoor: row?.angemaaktDoor?.value,
        gewijzigdDoor: row?.gewijzigdDoor?.value,
        statusLabel: row?.statusLabel?.value,
        datumZitting: row?.datumZitting?.value,
        link: row?.link?.value      };
      acc[row?.s?.value] = Object.assign(dataPart, dataPart1[row?.s?.value]);
      return acc;
    }, {});

    // Get the submissions date of publication
    const queryStringPart3 = `
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX eli: <http://data.europa.eu/eli/ontology#>

      SELECT 
        ?s
        ?datumPublicatie
       WHERE {
        GRAPH ?g {
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            prov:generated ?formData .
          FILTER ( ?verstuurd >= "${dateFrom}"^^xsd:dateTime )
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData eli:date_publication ?datumPublicatie .
          }
        }        
      }
    `;

    const queryResponsePart3 = await batchedQuery(queryStringPart3);
    const dataPart3 = queryResponsePart3.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        datumPublicatie: row?.datumPublicatie?.value
      };
      acc[row?.s?.value] = Object.assign(dataPart, dataPart2[row?.s?.value]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart3), [
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
      'datumPublicatie',
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
