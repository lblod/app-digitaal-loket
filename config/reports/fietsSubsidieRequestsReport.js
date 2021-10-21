import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'fietsSubsidieRequestsReport',
  execute: async () => {
    const reportData = {
      title: 'List of requests of fiets subsidies',
      description: 'All requests for bike subsidies that have been sent with their related information',
      filePrefix: 'fietsSubsidieRequestsReport'
    };
    console.log('Generate Fiets Subsidie Requests Report');
    const queryString = `
      PREFIX pav: <http://purl.org/pav/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX owl: <http://www.w3.org/2007/uwa/context/common.owl#>
      PREFIX qb: <http://purl.org/linked-data/cube#>

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?subsidiemaatregelConsumptie ?subsidiemaatregelConsumptieStatus ?formStatus ?iban
      WHERE {
        ?subsidiemaatregelConsumptie
          transactie:isInstantieVan <http://lblod.data.gift/concepts/70cc4947-33a3-4d26-82e0-2e1eacd2fea2> .

        OPTIONAL {
          ?subsidiemaatregelConsumptie owl:active/qb:order ?activeStepOrder .
        }
        FILTER ( ?activeStepOrder > 0 || NOT EXISTS { ?subsidiemaatregelConsumptie owl:active/qb:order ?activeStepOrder . } )

        OPTIONAL {
          ?subsidiemaatregelConsumptie adms:status/skos:prefLabel ?subsidiemaatregelConsumptieStatus ;
            dct:modified ?submissionDate ;
            m8g:hasParticipation ?participation ;
            dct:source ?applicationForm .
          ?bestuur m8g:playsRole ?participation ;
            skos:prefLabel ?bestuurseenheid .

          ?applicationForm adms:status/skos:prefLabel ?formStatus .

          OPTIONAL { ?applicationForm <http://schema.org/bankAccount>/<http://schema.org/identifier> ?iban . }

          FILTER EXISTS {
            ?applicationForm dct:isPartOf ?step .
            ?step dct:references <http://data.lblod.info/id/subsidieprocedurestappen/52f0b7dd244e42e0cda83804508e2e89d94ed098f3df8b4f9913a14f2be2423d> .
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
        subsidiemaatregelConsumptie: getSafeValue(subsidie, 'subsidiemaatregelConsumptie'),
        subsidiemaatregelConsumptieStatus: getSafeValue(subsidie, 'subsidiemaatregelConsumptieStatus'),
        formStatus: getSafeValue(subsidie, 'formStatus'),
        iban: getSafeValue(subsidie, 'iban'),
      };
    });

    await generateReportFromData(data, [
      'submissionDate',
      'bestuurseenheid',
      'subsidiemaatregelConsumptie',
      'subsidiemaatregelConsumptieStatus',
      'formStatus',
      'iban'
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
