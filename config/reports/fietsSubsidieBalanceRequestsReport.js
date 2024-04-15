import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'fietsSubsidieBalanceRequestsReport',
  execute: async () => {
    const reportData = {
      title: 'List of balance requests for fiets subsidies',
      description: 'All balance requests for bike subsidies that have been sent with their related information',
      filePrefix: 'fietsSubsidieBalanceRequestsReport'
    };
    console.log('Generate Fiets Subsidie Balance Requests Report');
    const queryString = `
      PREFIX pav: <http://purl.org/pav/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?subsidie ?subsidieStatus ?formStatus ?iban ?formStatusLabel ?dossierNummer
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> ;
            dct:source ?form .
          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/ac33d0fe-ff5a-41ea-8730-1d655cca8cce> ;
            adms:status ?formStatus .

          VALUES ?formStatus {
            <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c>
            <http://lblod.data.gift/concepts/79a52da4-f491-4e2f-9374-89a13cde8ecd>
          }

          ?formStatus skos:prefLabel ?formStatusLabel.

          OPTIONAL {
            ?subsidie adms:status/skos:prefLabel ?subsidieStatus ;
              dct:modified ?submissionDate ;
              m8g:hasParticipation ?participation .
            ?bestuur m8g:playsRole ?participation ;
              skos:prefLabel ?bestuurseenheid .
            OPTIONAL { ?form <http://schema.org/bankAccount>/<http://schema.org/identifier> ?iban . }
            OPTIONAL { ?form <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#identifier> ?dossierNummer. }
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm.
            ?anyForm dct:isPartOf ?step.
          }
        }
      }
      ORDER BY DESC(?submissionDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        submissionDate: subsidie?.submissionDate?.value,
        bestuurseenheid: subsidie?.bestuurseenheid?.value,
        subsidie: subsidie?.subsidie?.value,
        subsidieStatus: subsidie?.subsidieStatus?.value,
        formStatus: subsidie?.formStatusLabel?.value,
        iban: subsidie?.iban?.value,
        dossierNummer: subsidie?.dossierNummer?.value
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'submissionDate',
      'bestuurseenheid',
      'subsidieStatus',
      'formStatus',
      'iban',
      'dossierNummer'
    ], reportData);
  }
};
