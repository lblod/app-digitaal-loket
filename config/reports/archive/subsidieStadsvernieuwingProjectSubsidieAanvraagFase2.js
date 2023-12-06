import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 20 1 1 2023',
  name: 'subsidieStadsvernieuwingProjectSubsidieAanvraagFase2',
  execute: async () => {
    const reportData = {
      title: 'Subsidie Stadsvernieuwing Aanvraag (Fase 2) Report',
      description: 'List of bestuurseenheden that are applying/have applied for the Subsidie Stadsvernieuwing Aanvraag (Fase 2) subsidy',
      filePrefix: 'subsidieStadsvernieuwingProjectSubsidieAanvraagFase2'
    };

    console.log('Generate Subsidie Stadsvernieuwing Aanvraag (Fase 2) Report');

    const queryString = `
      PREFIX subsidie:    <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie:  <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit:     <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g:         <http://data.europa.eu/m8g/>
      PREFIX dct:         <http://purl.org/dc/terms/>
      PREFIX adms:        <http://www.w3.org/ns/adms#>
      PREFIX skos:        <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema:      <http://schema.org/>

      SELECT DISTINCT ?subsidie ?modified ?status ?bestuurseenheid ?classificatie
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/c10fc76f-effe-430e-822f-60f03f575302> ;
            dct:source ?form .
          
          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/12e775fc-163e-41f6-978a-041b299c6769> ;
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
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/c10fc76f-effe-430e-822f-60f03f575302> .
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
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        classificatie: getSafeValue(subsidie, 'classificatie')
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'modified',
      'status',
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
``
