import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'lekp21OproepIndienenPactReport',
  execute: async () => {
    const reportData = {
      title: 'List of pacts for LEKP 2.1',
      description: 'All pacts for LEKP 2.1 climate subsidy that have been sent with their related information',
      filePrefix: 'lekp21OproepIndienenPactReport'
    };
    
    console.log('Generate LEKP 2.1 Oproep 2023-2024 Opvolgmoment Report');

    const queryString = `
      PREFIX subsidie:    <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie:  <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit:     <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g:         <http://data.europa.eu/m8g/>
      PREFIX dct:         <http://purl.org/dc/terms/>
      PREFIX adms:        <http://www.w3.org/ns/adms#>
      PREFIX skos:        <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema:      <http://schema.org/>

      SELECT DISTINCT ?subsidie ?modified ?status ?contactFirstName ?contactLastName ?contactEmail ?contactTelephone
                      ?bestuurseenheid ?classificatie
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://data.lblod.info/id/subsidy-measure-offers/8815fe07-e53f-4657-94be-efaa4898f03c> ;
            dct:source ?form .
          
          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/a9a20ff7-a727-4b92-8d16-dac36b0c27cd> ;
            adms:status/skos:prefLabel ?status .
          
          OPTIONAL {
            ?subsidie m8g:hasParticipation ?participation ;
              dct:source ?form .

            ?participation m8g:role <http://lblod.data.gift/concepts/d8b8f3d1-7574-4baf-94df-188a7bd84a3a> .

            ?bestuurseenheidURI skos:prefLabel ?bestuurseenheid ;
              m8g:playsRole ?participation ;
              besluit:classificatie ?classURI .

            ?classURI skos:prefLabel ?classificatie .

            ?form dct:modified ?modified .

            OPTIONAL {
              ?form schema:contactPoint ?contactPoint .
              OPTIONAL { ?contactPoint foaf:firstName ?contactFirstName . }
              OPTIONAL { ?contactPoint foaf:familyName ?contactLastName . }
              OPTIONAL { ?contactPoint schema:email ?contactEmail . }
              OPTIONAL { ?contactPoint schema:telephone ?contactTelephone . }
            }
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://data.lblod.info/id/subsidy-measure-offers/8815fe07-e53f-4657-94be-efaa4898f03c> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm .
            ?anyForm dct:isPartOf ?step .
          }
        }
      }
      ORDER BY DESC(?modified)
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        subsidie: getSafeValue(subsidie, 'subsidie'),
        modified: getSafeValue(subsidie, 'modified'),
        status: getSafeValue(subsidie, 'status'),
        contactFirstName: getSafeValue(subsidie, 'contactFirstName'),
        contactLastName: getSafeValue(subsidie, 'contactLastName'),
        contactEmail: getSafeValue(subsidie, 'contactEmail'),
        contactTelephone: getSafeValue(subsidie, 'contactTelephone'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        classificatie: getSafeValue(subsidie, 'classificatie')
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'modified',
      'status',
      'contactFirstName',
      'contactLastName',
      'contactEmail',
      'contactTelephone',
      'bestuurseenheid',
      'classificatie'
    ], reportData);
  }
};

function getSafeValue(entry, property){
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain commas; wrapping them in escapes quotes doesn't disrupt the columns.
function wrapInQuote(value) {
  return `\"${value}\"`;
}
