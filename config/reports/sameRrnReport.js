import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 15 23 15 * *',
  name: 'sameRRN',
  execute: async () => {
    const reportData = {
      title: 'Lijst van dubbele personen',
      description: 'Persons with the same RRN but a different URI',
      filePrefix: 'sameRRN'
    };
    console.log('Generate duplicate RRN report');
    const queryString = `
      PREFIX person: <http://www.w3.org/ns/person#>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT ?person1 ?person2 WHERE {
        ?person1 a person:Person.
        ?person2 a person:Person.
        ?person1 adms:identifier/skos:notation ?rrn.
        ?person2 adms:identifier/skos:notation ?rrn.
        FILTER (?person1 != ?person2)
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => ({
      person1: row.person1.value,
      person2: row.person2.value,
    }));
    await generateReportFromData(data, ['person1', 'person2'], reportData);
  }
};
