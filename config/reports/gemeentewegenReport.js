import {generateReportFromData, batchedQuery} from '../helpers.js';

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'gemeentewegenReport',
  execute: async () => {
    const reportData = {
      title: 'List of the gemeentewegen submissions of the Toezicht part of Loket',
      description: 'Gemeentewegen submissions with some information.',
      filePrefix: 'gemeentewegenReport'
    };

    const dateFrom = "2020-09-01T00:00:00.000Z";
    const vaststellingGemeentewegType = "https://data.vlaanderen.be/id/concept/BesluitType/f1fd8f88-95b0-4085-b766-008b5867d992";

    console.log('Generate Gemeentewegen Submissions Report');

    // Get the submissions and uploaded files
    const queryStringPart1 = `
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

      SELECT ?s ?file WHERE { 
        GRAPH ?g { 
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            prov:generated ?formData .
          ?formData ext:decisionType <${vaststellingGemeentewegType}> ;
            dct:hasPart ?attachmentFile .
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
      PREFIX lblodBesluit: <http://lblod.data.gift/vocabularies/besluit/> 
      PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

      SELECT ?s ?verstuurd ?bestuurseenheidLabel ?typeBestuur ?datumZitting ?statusLabel
            ?angemaaktDoor ?gewijzigdDoor ?link ?typeVaststelling ?typeGemeenteweg
      WHERE { 
        GRAPH ?g { 
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            pav:createdBy ?bestuurseenheid ;
            adms:status ?status ;
            prov:generated ?formData .
          ?formData ext:decisionType <${vaststellingGemeentewegType}> ;
            ext:sessionStartedAtTime ?datumZitting .

          FILTER ( ?verstuurd >= "${dateFrom}"^^xsd:dateTime )
        }
        GRAPH ?h {
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
            ?formData dct:hasPart ?attachmentLink .
          }
          GRAPH ?h {
            ?attachmentLink a nfo:RemoteDataObject ;
              nie:url ?link .
          }
        }
        OPTIONAL {
          GRAPH ?g {
            ?formData lblodBesluit:AdoptionType ?adoptionType .
            ?formData lblodBesluit:MunicipalRoadProcedureType ?municipalRoadProcedureType .
          }
          GRAPH ?h {
            ?adoptionType skos:prefLabel ?typeVaststelling .
            ?municipalRoadProcedureType skos:prefLabel ?typeGemeenteweg .
          }
        }
      }
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        verstuurd: row?.verstuurd?.value,
        bestuurseenheidLabel: row?.bestuurseenheidLabel?.value,
        typeBestuur: row?.typeBestuur?.value,
        angemaaktDoor: row?.angemaaktDoor?.value,
        gewijzigdDoor: row?.gewijzigdDoor?.value,
        statusLabel: row?.statusLabel?.value,
        datumZitting: row?.datumZitting?.value,
        typeVaststelling: row?.typeVaststelling?.value,
        typeGemeenteweg: row?.typeGemeenteweg?.value,
        link: row?.link?.value,
      };
      acc[row?.s?.value] = Object.assign(dataPart, dataPart1[row?.s?.value]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart2), [
      'verstuurd',
      'bestuurseenheidLabel',
      'typeBestuur',
      'angemaaktDoor',
      'gewijzigdDoor',
      'statusLabel',
      'datumZitting',
      'typeVaststelling',
      'typeGemeenteweg',
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
