import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 40 23 * * *',
  name: 'instancesStuckinPublishingForLPDCReport',
  execute: async () => {
    const reportData = {
      title: 'List of LPDC instances stuck in publishing',
      description: 'All instances (public services and tombstones) that are sent but stuck in publishing',
      filePrefix: 'instancesStuckinPublishingForLPDCReport'
    };
    
    console.log('Generate list of stuck LPDC instances');

    const queryString = `
    PREFIX cpsv:    <http://purl.org/vocab/cpsv#>
    PREFIX schema:  <http://schema.org/>
    PREFIX adms:    <http://www.w3.org/ns/adms#>
    PREFIX as:      <https://www.w3.org/ns/activitystreams#>
    
    SELECT DISTINCT ?publicService WHERE {
      {
        GRAPH ?graph {
          ?publicService a cpsv:PublicService ;
            adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        }
        FILTER NOT EXISTS {
          ?publicService schema:publication <http://lblod.data.gift/concepts/3369bb10-1962-11ed-b07c-132292303e92> .
        }
      }
      UNION {
        GRAPH ?graph {
          ?publicService a cpsv:PublicService ;
            adms:status         <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> ;
            schema:publication  <http://lblod.data.gift/concepts/a7d01120-6f93-11ed-bcb8-a144c50c46d7> .
        }
      }
      UNION {
        GRAPH ?graph {
          ?publicService a as:Tombstone ;
            as:formerType       cpsv:PublicService ;
            schema:publication  <http://lblod.data.gift/concepts/a7d01120-6f93-11ed-bcb8-a144c50c46d7> .
        }
      }
    }
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((publicService) => {
      return {
        publicService: getSafeValue(publicService, 'publicService')
      };
    });

    await generateReportFromData(data, [
      'publicService'
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
