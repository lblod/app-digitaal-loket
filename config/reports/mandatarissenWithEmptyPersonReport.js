import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 22 * * *',
  name: 'mandatarissenWithEmptyPersonReport',
  execute: async () => {
    const reportData = {
      title: 'List mandatarissen linked to an empty person',
      description: 'Mandatarissen and the uris of the empty persons.',
      filePrefix: 'mandatarissenWithEmptyPerson'
    };
    console.log('Generate mandatarissenWithEmptyPerson Report');
    const queryString = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX org: <http://www.w3.org/ns/org#>

      SELECT DISTINCT ?mandataris ?start ?end ?role ?person
      WHERE {
        ?mandataris a mandaat:Mandataris ;
          mandaat:isBestuurlijkeAliasVan ?person .

        OPTIONAL { ?mandataris mandaat:start ?start }
        OPTIONAL { ?mandataris mandaat:einde ?end }
        OPTIONAL { ?mandataris org:holds/org:role/skos:prefLabel ?role }

        FILTER NOT EXISTS { ?person ?p ?o }
      }
      ORDER BY ?mandataris
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => {
      return {
        mandataris: result.mandataris.value,
        start: result?.start?.value,
        end: result?.end?.value,
        role: result?.role?.value,
        person: result.person.value
      };
    });

    await generateReportFromData(data, ['mandataris', 'start', 'end', 'role', 'person'], reportData);
  }
};
