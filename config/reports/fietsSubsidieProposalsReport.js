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

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?subsidie ?subsidieStatus ?formStatus ?dossierNummer
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> ;
            dct:source ?form .
          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidieprocedurestappen/002f93ed-bdb0-4e3a-af13-ef6c00e89651> .
          OPTIONAL {
            ?subsidie adms:status/skos:prefLabel ?subsidieStatus ;
              dct:modified ?submissionDate ;
              m8g:hasParticipation ?participation ;
              dct:source ?form .
            ?bestuur m8g:playsRole ?participation ;
              skos:prefLabel ?bestuurseenheid .

            ?form adms:status/skos:prefLabel ?formStatus .
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
        submissionDate: getSafeValue(subsidie, 'submissionDate'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        subsidie: getSafeValue(subsidie, 'subsidie'),
        subsidieStatus: getSafeValue(subsidie, 'subsidieStatus'),
        formStatus: getSafeValue(subsidie, 'formStatus'),
        dossierNummer: getSafeValue(subsidie, 'dossierNummer')
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'submissionDate',
      'bestuurseenheid',
      'subsidieStatus',
      'formStatus',
      'dossierNummer'
    ], reportData);
  }
};

function getSafeValue(entry, property){
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
