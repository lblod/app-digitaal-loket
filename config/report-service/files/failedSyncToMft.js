import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'failedSyncToMft',
  execute: async () => {
    const reportData = {
      title: 'Number of failed synchronizations to MFT',
      description: 'Number of failed synchronizations to MFT',
      filePrefix: 'MftFailedSync'
    };
    console.log('Generate failedSyncToMft Report');
    const queryString = `
      PREFIX bbcdr: <http://mu.semte.ch/vocabularies/ext/bbcdr/>
      SELECT (COUNT(?uri) AS ?numberOfFailedSync) WHERE {
        ?uri bbcdr:status <http://mu.semte.ch/vocabularies/ext/bbcdr-status/DELIVERY_FAILED>.
      }
    `;
    const queryResponse = await query(queryString);
    console.log('queryResponse: '+queryResponse);
    const data = queryResponse.results.bindings.map((row) => {
      console.log(row);
      return {
        numberOfFailedSync: row.numberOfFailedSync.value,
      };
    });
    await generateReportFromData(data, ['numberOfFailedSync'], reportData);
  }
};
