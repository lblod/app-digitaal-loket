import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'submissions',
  execute: async () => {
    const reportData = {
      title: 'List of the submissions of the Toezicht part of Loket',
      description: 'Submissions with their date, the type of submission, the unit, the type of unit and the date of the meeting.',
      filePrefix: 'submissions'
    };
    console.log('Generate Submissions Report');
    const queryString = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX pav: <http://purl.org/pav/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>

      SELECT ?verstuurd ?typeDossier ?bestuurseenheidLabel ?typeBestuur ?datumZitting WHERE { 
        GRAPH ?g { 
          ?s a meb:Submission ;
            nmo:sentDate ?verstuurd ;
            pav:createdBy ?bestuurseenheid ;
            prov:generated ?formData .
          ?formData ext:decisionType ?type ;
            ext:sessionStartedAtTime ?datumZitting .
        }

        GRAPH ?h {
          ?type skos:prefLabel ?typeDossier .
          ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel ;
            besluit:classificatie/skos:prefLabel ?typeBestuur .
        }
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((submission) => {
      return {
        verstuurd: submission.verstuurd.value ? submission.verstuurd.value : '',
        typeDossier: submission.typeDossier.value,
        bestuurseenheidLabel: submission.bestuurseenheidLabel.value,
        typeBestuur: submission.typeBestuur.value,
        datumZitting: submission.datumZitting.value
      };
    });

    await generateReportFromData(data, ['verstuurd', 'typeDossier', 'bestuurseenheidLabel', 'typeBestuur', 'datumZitting'], reportData);
  }
};
