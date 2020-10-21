import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 30 23 * * *',
  name: 'mandatarissenWithoutStatusReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen having no status',
      description: 'Mandatarissen with their first name and last name, role and bestuurseenheid.',
      filePrefix: 'mandatarissenWithoutStatus'
    };
    console.log('Generate mandatarissenWithoutStatus Report');
    const queryString = `
      SELECT DISTINCT ?mandataris ?firstName ?lastName ?role ?start ?end ?bestuurseenheid
      WHERE {
        ?mandataris a <http://data.vlaanderen.be/ns/mandaat#Mandataris> ;
          <http://www.w3.org/ns/org#holds> ?mandate ;
          <http://data.vlaanderen.be/ns/mandaat#start> ?start ;
          <http://data.vlaanderen.be/ns/mandaat#einde> ?end ;
          <http://data.vlaanderen.be/ns/mandaat#isBestuurlijkeAliasVan> ?person .
        ?person <http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam> ?firstName ;
          <http://xmlns.com/foaf/0.1/familyName> ?lastName .
        ?mandate <http://www.w3.org/ns/org#role> ?roleUri .
        ?bestuursorgaanInTijd <http://www.w3.org/ns/org#hasPost> ?mandate ;
          <http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan> ?bestuursorgaan .
        ?bestuursorgaan <http://data.vlaanderen.be/ns/besluit#bestuurt> ?bestuurseenheidUri .
        ?bestuurseenheidUri skos:prefLabel ?bestuurseenheid .
        ?roleUri skos:prefLabel ?role .
        FILTER NOT EXISTS { ?mandataris <http://data.vlaanderen.be/ns/mandaat#status> ?status }
      }
      ORDER BY ?bestuurseenheid
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        mandataris: result.mandataris.value,
        firstName: result.firstName.value,
        lastName: result.lastName.value,
        role: result.role.value,
        start: result.start.value,
        end: result.end.value,
        bestuurseenheid: result.bestuurseenheid.value
      };
    });

    await generateReportFromData(data, ['mandataris', 'firstName', 'lastName', 'role', 'start', 'end', 'bestuurseenheid'], reportData);
  }
};
