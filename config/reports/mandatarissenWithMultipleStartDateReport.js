import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 22 * * 6',
  name: 'mandatarissenWithMultipleStartDateReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen having two start dates',
      description: 'Mandatarissen with their start dates, first name and last name.',
      filePrefix: 'mandatarissenWithMultipleStartDate'
    };
    console.log('Generate mandatarissenWithMultipleStartDate Report');
    const queryString = `
      SELECT DISTINCT ?mandataris ?startDate1 ?startDate2 ?firstName ?lastName
      WHERE {
        ?mandataris a <http://data.vlaanderen.be/ns/mandaat#Mandataris> ; 
          <http://data.vlaanderen.be/ns/mandaat#start> ?startDate1, ?startDate2 ;
          <http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan> ?person .
        ?person <http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam> ?firstName ;
          <http://xmlns.com/foaf/0.1/familyName> ?lastName .
        FILTER(?startDate1 != ?startDate2)
        FILTER(?startDate1 < ?startDate2)
      }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((submission) => {
      return {
        mandataris: submission.mandataris.value,
        startDate1: submission.startDate1.value,
        startDate2: submission.startDate2.value,
        firstName: submission.firstName.value,
        lastName: submission.lastName.value
      };
    });

    await generateReportFromData(data, ['mandataris', 'startDate1', 'startDate2', 'firstName', 'lastName'], reportData);
  }
};
