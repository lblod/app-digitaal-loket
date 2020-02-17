import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'inzendingenReport',
  execute: async () => {
    const reportData = {
      title: 'Inzendingen Report',
      description: 'Number of inzendingen by decision type',
      filePrefix: 'inzendingen'
    };
    console.log('Generate Inzendingen Report');
    const queryString = `
      PREFIX toezicht: <http://mu.semte.ch/vocabularies/ext/supervision/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

      select ?type (COUNT(?type) as ?typeCount) where {
        GRAPH ?g {
          ?uri a toezicht:InzendingVoorToezicht;
            toezicht:decisionType ?typeURI.
        }
        GRAPH ?h {
          ?typeURI skos:prefLabel ?type.
        }
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((inzendingen) => ({
      type: inzendingen.type.value,
      typeCount: inzendingen.typeCount.value,
    }));
    await generateReportFromData(data, ['type', 'typeCount'], reportData);
  }
};
