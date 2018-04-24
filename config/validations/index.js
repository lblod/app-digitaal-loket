import { query } from 'mu';
import { validateSparqlSelect } from '/app/helpers';

const graph = process.env.MU_APPLICATION_GRAPH;

const validations = [
  {
    name: 'Start required on mandataris',
    description: 'Start is een verplicht attribuut van mandataris',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: 'Start is een verplicht attribuut van mandataris',
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        SELECT ?s
        FROM <${graph}>
        WHERE {
          ?s a mandaat:Mandataris .
          FILTER(NOT EXISTS { ?s mandaat:start ?o . })
        }`)
  },
  {
    name: 'Bestuurlijke alias required on mandataris',
    description: 'Bestuurlijke alias is een verplichte relatie van mandataris',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: 'Bestuurlijke alias is een verplichte relatie van mandataris',
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        SELECT ?s
        FROM <${graph}>
        WHERE {
          ?s a mandaat:Mandataris .
          FILTER(NOT EXISTS { ?s mandaat:isBestuurlijkeAliasVan ?o . })
        }`)
  },
  {
    name: 'Mandataris start before end',
    description: 'Start van mandataris moet voor einde vallen',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: function(params) {
      return `Mandataris ${params['s']}: start ${params['start']} is later dan einde ${params['einde']}`;
    },
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/#>
        SELECT ?s ?start ?einde
        FROM <${graph}>
        WHERE {
          ?s a mandaat:Mandataris ;
             mandaat:start ?start ;
             mandaat:einde ?einde .
          FILTER (?einde < ?start)
        }`)
  },
  {
    name: 'Aantal houders required on mandaat',
    description: 'Aantal houders is een verplicht attribuut van mandaat',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: 'Aantal houders is een verplicht attribuut van mandaat',
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        SELECT ?s
        FROM <${graph}>
        WHERE {
          ?s a mandaat:Mandaat .
          FILTER(NOT EXISTS { ?s mandaat:aantalHouders ?o . })
        }`)
  },  
  {
    name: 'Max. aantal houders of mandate',
    description: 'Maximaal aantal houders van mandaat niet overschreden',
    validationSets: [
      'http://data.lblod.info/id/validation-set/mandatendatabank'
    ],
    message: function(params) {
      return `${params['count']} houders van mandaat ${params['s']}, terwijl er maximaal ${params['max']} toegelated zijn.`;
    },
    validate: validateSparqlSelect(`
        PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
        PREFIX org: <http://www.w3.org/ns/org#>
        FROM <${graph}>
        SELECT (COUNT(?m) as ?count) ?s ?max 
        WHERE {
          ?s a mandaat:Mandaat ;
             mandaat:aantalHouders ?max .
          ?m org:holds ?s .
        } 
        GROUP BY ?s ?max
        HAVING (?max < COUNT(?m))
    `)
  }
];

export default validations;
