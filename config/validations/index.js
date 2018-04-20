import { query } from 'mu';
import { validateSparqlSelect, validateSparqlAsk, insertNewError } from '/app/helpers';

const graph = process.env.MU_APPLICATION_GRAPH;

const validations = [
  {
    name: 'at-least-1-mandataris',
    description: 'At least 1 mandataris must be defined',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: 'Er moet minstens 1 mandataris opgeslagen zijn',
    validate: validateSparqlAsk(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        ASK {
          GRAPH <${graph}> {
            ?s a mandaat:Mandataris .
          }
        }`)
  },
  {
    name: 'totaal-aantal-mandaten',
    description: 'Totaal aantal mandaten mag niet hoger zijn dan 10000',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message() { return this.description; },
    validate: async function(execution) {
      const queryResult = await query(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        SELECT COUNT(?s) as ?count 
        FROM <${graph}>
        WHERE { ?s a mandaat:Mandaat }`);
      const count = parseInt(queryResult.results.bindings[0].count.value);
      if (count > 10000) {
        await insertNewError(execution.uri, this.uri, this.message());
        return false;
      }
      return true;
    }
  },
  {
    name: 'mandataris-start-before-end',
    description: 'Start date must fall before end date',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: function(params) {
      return `Mandataris ${params['s']}: start ${params['start']} is later dan einde ${params['einde']}`;
    },
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/#>
        SELECT ?s ?uuid ?start ?einde
        FROM <${graph}>
        WHERE {
          ?s a mandaat:Mandataris ;
             mandaat:start ?start ;
             mandaat:einde ?einde .
          OPTIONAL { ?s mu:uuid ?uuid } 
          FILTER (?einde < ?start)
        }`)
  }
];

export default validations;
