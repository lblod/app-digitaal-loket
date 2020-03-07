import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 2 * * *',
  name: 'bestuurseenhedenWithoutNotificationEmail',
  execute: async () => {
    const reportData = {
      title: 'Bestuurseenheden zonder notificatie emailadres in Berichtencentrum',
      description: 'Report listing all bestuurseenheden without notification emailaddress in Berichtencentrum',
      filePrefix: 'bestuurseenhedenWithoutNotificationEmail'
    };
    console.log('Generate bestuurseenhedenWithoutNotificationEmail Report');
    const queryString = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>

      SELECT ?bestuurseenheid ?type ?kbo WHERE {
        GRAPH ?g {
          ?s a besluit:Bestuurseenheid ; skos:prefLabel ?bestuurseenheid ; besluit:classificatie ?classif ; ext:kbonummer ?kbo .
          ?classif skos:prefLabel ?type .
        }
        GRAPH ?h {
          FILTER NOT EXISTS { ?s ext:mailAdresVoorNotificaties ?emailAddress . }
        }
      }
    `;
    const queryResponse = await query(queryString);
    console.log('queryResponse: '+queryResponse);
    const data = queryResponse.results.bindings.map((row) => {
      console.log(row);
      return {
        bestuurseenheid: row.bestuurseenheid.value,
        type: row.type.value,
        kbo: row.kbo.value
      };
    });
    await generateReportFromData(data, ['bestuurseenheid', 'type', 'kbo'], reportData);
  }
};
