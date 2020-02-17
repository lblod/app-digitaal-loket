import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'sameRRN',
  execute: async () => {
    const reportData = {
      title: 'Same RRN but different URI Report',
      description: 'Number of persons with same RRN but different URI',
      filePrefix: 'sameRRN'
    };
    console.log('Generate BBC-DR Dossiers Report');
    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX dct: <http://purl.org/dc/terms/>
      SELECT DISTINCT (COUNT(?person1) AS ?sameRRN) WHERE {
        ?person1 a foaf:Person.
        ?person2 a foaf:Person.
        ?person1 dct:identifier ?rrn.
        ?person2 dct:identifier ?rrn.
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => ({
      sameRRN: row.sameRRN.value,
    }));
    await generateReportFromData(data, ['sameRRN'], reportData);
  }
};
