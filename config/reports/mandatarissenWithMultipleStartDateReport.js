import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 30 22 * * *',
  name: 'mandatarissenWithMultipleStartDateReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen having two start dates',
      description: 'Mandatarissen with their start dates, first name and last name.',
      filePrefix: 'mandatarissenWithMultipleStartDate'
    };
    console.log('Generate mandatarissenWithMultipleStartDate Report');
    const queryString = `
      SELECT DISTINCT ?mandataris ?startDate ?firstName ?lastName
      WHERE {
        ?mandataris a <http://data.vlaanderen.be/ns/mandaat#Mandataris> ; 
          <http://data.vlaanderen.be/ns/mandaat#start> ?startDate, ?otherStartDate ;
          <http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan> ?person .
        ?person <http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam> ?firstName ;
          <http://xmlns.com/foaf/0.1/familyName> ?lastName .
        FILTER(str(?startDate) != str(?otherStartDate))
      }
      ORDER BY ?mandataris
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        mandataris: result.mandataris.value,
        startDate: result.startDate.value,
        firstName: result.firstName.value,
        lastName: result.lastName.value
      };
    });

    await generateReportFromData(data, ['mandataris', 'startDate', 'firstName', 'lastName'], reportData);
  }
};
