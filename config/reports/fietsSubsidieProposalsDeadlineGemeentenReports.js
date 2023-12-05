import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';
import { getSafeValue } from './util/report-helpers';

export default {
  cronPattern: '0 0 20 1 1 2023',
  name: 'fietsSubsidieProposalsDeadlineGemeentenReport',
  execute: async () => {
    const reportData = {
      title: 'List of proposals of fiets subsidies for checking deadline gemeenten',
      description: 'All proposals for bike subsidies of Gemeenten with submission date information',
      filePrefix: 'fietsSubsidieProposalsDeadlineGemeentenReport'
    };
    console.log('Generate Fiets Subsidie Proposals Deadline Gemeenten Report');
    const queryString = `
    PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
    PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
    PREFIX m8g: <http://data.europa.eu/m8g/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX adms: <http://www.w3.org/ns/adms#>

    SELECT DISTINCT ?subsidie ?bestuurseenheid ?subsidieStatus ?formStatus ?dossierNummer ?modifiedStep
    WHERE {
        ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
        transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> ;
            dct:source ?form .

        ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidieprocedurestappen/002f93ed-bdb0-4e3a-af13-ef6c00e89651> .
        ?form adms:status/skos:prefLabel ?formStatus ;
            dct:modified ?modifiedStep.
        FILTER (?modifiedStep > "2022-10-15T21:59:00Z"^^xsd:dateTime)
        ?subsidie adms:status/skos:prefLabel ?subsidieStatus ;
            m8g:hasParticipation ?participation .
        ?bestuur <http://data.vlaanderen.be/ns/besluit#classificatie> <http://data.vlaanderen.be/id/concept/BestuurseenheidClassificatieCode/5ab0e9b8a3b2ca7c5e000001>;
            m8g:playsRole ?participation ;
            skos:prefLabel ?bestuurseenheid .
        OPTIONAL { ?form <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#identifier> ?dossierNummer. }
    }
    ORDER BY DESC(?modifiedStep)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        subsidie: getSafeValue(subsidie, 'subsidie'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        modified: getSafeValue(subsidie, 'modifiedStep'),
        subsidieStatus: getSafeValue(subsidie, 'subsidieStatus'),
        formStatus: getSafeValue(subsidie, 'formStatus'),
        dossierNummer: getSafeValue(subsidie, 'dossierNummer'),
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'bestuurseenheid',
      'modified',
      'subsidieStatus',
      'formStatus',
      'dossierNummer'
    ], reportData);
  }
};
