import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 23 * * *',
  name: 'personenWithMultipleNamesReport',
  execute: async () => {
    const reportData = {
      title: 'List persons having two different first names',
      description: 'Persons with their first name and last name.',
      filePrefix: 'personenWithMultipleNames'
    };
    console.log('Generate personenWithMultipleNames Report');
    const queryString = `
      SELECT DISTINCT ?person ?firstName ?lastName
      WHERE {
        ?person a <http://www.w3.org/ns/person#Person> ;
          <http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam> ?firstName, ?differentFirstName ;
          <http://xmlns.com/foaf/0.1/familyName> ?lastName .
        filter(?firstName != ?differentFirstName)
      }
      ORDER BY ?person
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        person: result.person.value,
        firstName: result.firstName.value,
        lastName: result.lastName.value
      };
    });

    await generateReportFromData(data, ['person', 'firstName', 'lastName'], reportData);
  }
};
