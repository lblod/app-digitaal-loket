import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'fietsSubsidieProposalsReport',
  execute: async () => {
    const reportData = {
      title: 'List of proposals of fiets subsidies',
      description: 'All proposals for bike subsidies that have been sent with their related information',
      filePrefix: 'fietsSubsidieProposalsReport'
    };
    console.log('Generate Fiets Subsidie Proposals Report');
    const queryString = `
      PREFIX pav: <http://purl.org/pav/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?subsidiemaatregelConsumptie ?status
      WHERE {
        ?subsidiemaatregelConsumptie
          transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> ;
          adms:status/skos:prefLabel ?status ;
          dct:modified ?submissionDate ;
          m8g:hasParticipation ?participation .
        ?bestuur m8g:playsRole ?participation ;
          skos:prefLabel ?bestuurseenheid .

        FILTER EXISTS {
          ?subsidiemaatregelConsumptie dct:source ?applicationForm .
          ?applicationForm dct:isPartOf ?step .
          ?step dct:references <http://data.lblod.info/id/subsidieprocedurestappen/002f93ed-bdb0-4e3a-af13-ef6c00e89651> .
        }
      }
      ORDER BY DESC(?submissionDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        submissionDate: subsidie.submissionDate.value,
        bestuurseenheid: subsidie.bestuurseenheid.value,
        subsidiemaatregelConsumptie: subsidie.subsidiemaatregelConsumptie.value,
        status: subsidie.status.value
      };
    });

    await generateReportFromData(data, [
      'submissionDate',
      'bestuurseenheid',
      'subsidiemaatregelConsumptie',
      'status'
    ], reportData);
  }
};
