import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'climateSubsidiePactsReport',
  execute: async () => {
    const reportData = {
      title: 'List of pacts for climate subsidies',
      description: 'All pacts for climate subsidies that have been sent with their related information',
      filePrefix: 'climateSubsidiePactsReport'
    };
    console.log('Generate Climate Subsidie Pacts Report');
    const queryString = `
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema: <http://schema.org/>

      SELECT DISTINCT ?modified ?status ?contactFirstName ?contactLastName ?contactEmail ?contactTelephone
             ?bestuurseenheid ?classificatie
      WHERE {
        ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
          transactie:isInstantieVan
              <http://lblod.data.info/id/subsidy-measure-offers/64d40351-8128-464f-990f-41066154583e> ;
          m8g:hasParticipation ?participation ;
          dct:source ?form .

        ?participation m8g:role <http://lblod.data.gift/concepts/d8b8f3d1-7574-4baf-94df-188a7bd84a3a> .

        ?bestuurseenheidURI skos:prefLabel ?bestuurseenheid ;
          m8g:playsRole ?participation ;
          besluit:classificatie ?classURI .

        ?classURI skos:prefLabel ?classificatie .

        ?form dct:modified ?modified ;
          adms:status/skos:prefLabel ?status ;
          dct:isPartOf ?step .

        ?step dct:references
          <http://data.lblod.info/id/subsidy-procedural-steps/d6ec1fb1-a991-47ba-95f1-afdd87e4553c> .

        OPTIONAL { 
          ?form schema:contactPoint ?contactPoint .
          OPTIONAL { ?contactPoint foaf:firstName ?contactFirstName . }
          OPTIONAL { ?contactPoint foaf:familyName ?contactLastName . }
          OPTIONAL { ?contactPoint schema:email ?contactEmail . }
          OPTIONAL { ?contactPoint schema:telephone ?contactTelephone . }
        }

      }
      ORDER BY DESC(?modified)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
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

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
