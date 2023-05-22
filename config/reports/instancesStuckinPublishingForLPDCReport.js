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
    PREFIX dct:     <http://purl.org/dc/terms/>
    PREFIX pav:     <http://purl.org/pav/>
    PREFIX rdfs-ns: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
    PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>

    SELECT DISTINCT ?publicService ?type ?title ?bestuurseenheidLabel ?classificatieLabel WHERE {
      {
        GRAPH ?graph {
          ?publicService a cpsv:PublicService ;
            rdfs-ns:type  ?type ;
            dct:title     ?title ;
            pav:createdBy ?bestuurseenheid ;
            adms:status   <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

            FILTER(LANG(?title) = "nl")
        }

        GRAPH ?g {
          ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel ;
            besluit:classificatie ?classificatie .

          ?classificatie skos:prefLabel ?classificatieLabel .
        }

        FILTER NOT EXISTS {
          ?publicService schema:publication <http://lblod.data.gift/concepts/3369bb10-1962-11ed-b07c-132292303e92> .
        }
      }
      UNION {
        GRAPH ?graph {
          ?publicService a cpsv:PublicService ;
            rdfs-ns:type        ?type ;
            dct:title           ?title ;
            pav:createdBy       ?bestuurseenheid ;
            adms:status         <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> ;
            schema:publication  <http://lblod.data.gift/concepts/a7d01120-6f93-11ed-bcb8-a144c50c46d7> .

            FILTER(LANG(?title) = "nl")
        }

        GRAPH ?g {
          ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel ;
            besluit:classificatie ?classificatie .

          ?classificatie skos:prefLabel ?classificatieLabel .
        }
      }
      UNION {
        GRAPH ?graph {
          ?publicService a as:Tombstone ;
            rdfs-ns:type        ?type ;
            as:formerType       cpsv:PublicService ;
            schema:publication  <http://lblod.data.gift/concepts/a7d01120-6f93-11ed-bcb8-a144c50c46d7> .

          # Graphs have a consistent structure.
          # Ex: http://mu.semte.ch/graphs/organizations/8620c62b9e51d2275c98cb724ce4b6784b432db8e1e0376ac70cbda098ea0d0a/LoketLB-LPDCGebruiker
          BIND(STRBEFORE(STRAFTER(STR(?graph), "http://mu.semte.ch/graphs/organizations/"), "/LoketLB-LPDCGebruiker") as ?bestuurseenheidUUID)

        }

        GRAPH ?g {
          ?bestuurseenheid ?p ?bestuurseenheidUUID .
          ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel ;
            besluit:classificatie ?classificatie .

          ?classificatie skos:prefLabel ?classificatieLabel .
        }
      }
    }
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((publicService) => {
      return {
        publicService: getSafeValue(publicService, 'publicService'),
        type: getSafeValue(publicService, 'type'),
        title: getSafeValue(publicService, 'title'),
        bestuurseenheidLabel: getSafeValue(publicService, 'bestuurseenheidLabel'),
        classificatieLabel: getSafeValue(publicService, 'classificatieLabel'),
      };
    });

    await generateReportFromData(data, [
      'publicService',
      'type',
      'title',
      'bestuurseenheidLabel',
      'classificatieLabel',
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
