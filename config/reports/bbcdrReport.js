import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'bbcdrDossiers',
  execute: async () => {
    const reportData = {
      title: 'BBC-DR Dossiers Report',
      description: 'Number of BBC-DR Dossiers',
      filePrefix: 'bbcdrDossiers'
    };
    console.log('Generate BBC-DR Dossiers Report');
    const queryString = `
      SELECT DISTINCT  (COUNT(?uri) AS ?bbcdrReportCount) WHERE {
        ?uri a <http://mu.semte.ch/vocabularies/ext/bbcdr/Report>
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => ({
      bbcdrReportCount: row.bbcdrReportCount.value,
    }));
    await generateReportFromData(data, ['bbcdrReportCount'], reportData);
  }
};
