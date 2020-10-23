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
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT DISTINCT ?mandataris ?startDate ?firstName ?lastName
      WHERE {
        ?mandataris a mandaat:Mandataris ;
          mandaat:start ?startDate, ?otherStartDate ;
          mandaat:isBestuurlijkeAliasVan ?person .
        ?person persoon:gebruikteVoornaam ?firstName ;
          foaf:familyName ?lastName .
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
