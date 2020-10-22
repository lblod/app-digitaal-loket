import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 35 22 * * *',
  name: 'mandatarissenWithMultipleEndDateReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen having two end dates',
      description: 'Mandatarissen with their end dates, first name and last name.',
      filePrefix: 'mandatarissenWithMultipleEndDate'
    };
    console.log('Generate mandatarissenWithMultipleEndDate Report');
    const queryString = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT DISTINCT ?mandataris ?endDate ?firstName ?lastName
      WHERE {
        ?mandataris a mandaat:Mandataris ;
          mandaat:einde ?endDate, ?otherEndDate ;
          mandaat:isBestuurlijkeAliasVan ?person .
        ?person persoon:gebruikteVoornaam ?firstName ;
          foaf:familyName ?lastName .
        FILTER(str(?endDate) != str(?otherEndDate))
      }
      ORDER BY ?mandataris
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        mandataris: result.mandataris.value,
        endDate: result.endDate.value,
        firstName: result.firstName.value,
        lastName: result.lastName.value
      };
    });

    await generateReportFromData(data, ['mandataris', 'endDate', 'firstName', 'lastName'], reportData);
  }
};
