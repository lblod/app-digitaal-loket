import { query } from 'mu';
import { generateReportFromData } from '../helpers.js';

export default {
  cronPattern: '0 10 0 * * *',
  name: 'harvested-and-non-harvested-mandatarissen-report',
  execute: async () => {
    const reportData = {
      title: 'List of harvested and non-harvested mandatarissen having the same position',
      description: 'Selects harvested and non-harvested mandatarissen having the same position alongside the organization name',
      filePrefix: 'harvested-and-non-harvested-mandataris-for-same-position'
    };

    console.log('Generating report for detecting harvested and non-harvested mandatarissen having the same position');

    const queryString = `
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

      SELECT DISTINCT ?worshipService ?worshipServiceName ?subject ?subjectHarvested ?post WHERE {
        GRAPH <http://mu.semte.ch/graphs/public> {
          ?timedGoverningBody org:hasPost ?post ;
            <http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan> ?governingBody .

          ?governingBody besluit:bestuurt ?worshipService .

          ?worshipService skos:prefLabel ?worshipServiceName .
        }

        GRAPH ?g {
          ?subject org:holds ?post .
          FILTER NOT EXISTS { ?subject <http://www.w3.org/ns/prov#wasGeneratedBy> <http://lblod.data.gift/id/app/lblod-harvesting> . }

          ?subjectHarvested org:holds ?post .
          FILTER EXISTS { ?subjectHarvested <http://www.w3.org/ns/prov#wasGeneratedBy> <http://lblod.data.gift/id/app/lblod-harvesting> . }
        }
      } ORDER BY ?worshipServiceName
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((result) => ({
      worshipService: result.worshipService.value,
      worshipServiceName: result.worshipServiceName.value,
      mandatePositionLoket: result.subject.value,
      mandatePositionHarvested: result.subjectHarvested.value,
      governingBodyPost: result.post.value
    }));

    await generateReportFromData(data, ['worshipService', 'worshipServiceName', 'mandatePositionLoket', 'mandatePositionHarvested', 'governingBodyPost'], reportData);
  }
};
