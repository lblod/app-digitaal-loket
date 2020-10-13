import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'personenWithMultipleNamesReport',
  execute: async () => {
    const reportData = {
      title: 'List persons having two first names',
      description: 'Persons with their different first names and last names.',
      filePrefix: 'personenWithMultipleNames'
    };
    console.log('Generate personenWithMultipleNames Report');
    const queryString = `
      SELECT DISTINCT ?person ?firstName1 ?lastName1 ?firstName2 ?lastName2
      WHERE {
        ?person a <http://www.w3.org/ns/person#Person> ;
          <http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam> ?firstName1, ?firstName2 ;
          <http://xmlns.com/foaf/0.1/familyName> ?lastName1, ?lastName2 .
        filter(?firstName1 != ?firstName2)
        filter(?firstName1 <= ?firstName2)
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((submission) => {
      return {
        person: submission.person.value,
        firstName1: submission.firstName1.value,
        firstName2: submission.firstName2.value,
        lastName1: submission.lastName1.value,
        lastName2: submission.lastName2.value
      };
    });

    await generateReportFromData(data, ['person', 'firstName1', 'firstName2', 'lastName1', 'lastName2'], reportData);
  }
};
