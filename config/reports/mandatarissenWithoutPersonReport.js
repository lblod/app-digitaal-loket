import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 30 23 * * *',
  name: 'mandatarissenWithoutPersonReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen having no person linked',
      description: 'Mandatarissen having no person linked',
      filePrefix: 'mandatarissenWithoutPerson'
    };
    console.log('Generate mandatarissenWithoutPerson Report');
    const queryString = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>

      SELECT DISTINCT ?mandataris
      WHERE { 
        GRAPH ?g {
          ?mandataris a mandaat:Mandataris .
          FILTER NOT EXISTS { ?mandataris mandaat:isBestuurlijkeAliasVan ?person . }
        }
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        mandataris: result.mandataris.value
      };
    });

    await generateReportFromData(data, ['mandataris'], reportData);
  }
};
