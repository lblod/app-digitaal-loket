import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 15 23 15 * *',
  name: 'sameRRN',
  execute: async () => {
    const reportData = {
      title: 'Same RRN but different URI Report',
      description: 'Number of persons with same RRN but different URI',
      filePrefix: 'sameRRN'
    };
    console.log('Generate duplicate RRN report');
    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX dct: <http://purl.org/dc/terms/>
      SELECT DISTINCT ?person1 ?person2 WHERE {
        ?person1 a foaf:Person.
        ?person2 a foaf:Person.
        ?person1 dct:identifier ?rrn.
        ?person2 dct:identifier ?rrn.
        FILTER (?person1 != ?person2)
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => ({
      person1: row.person1.value,
      person2: row.person2.value,
    }));
    await generateReportFromData(data, ['sameRRN'], reportData);
  }
};
