import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'accountsReport',
  execute: async () => {
    const reportData = {
      title: 'Accounts Report',
      description: 'All accounts and the bestuurseenheid they belong',
      filePrefix: 'accounts'
    };
    console.log('Generate Account Report');
    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        SELECT DISTINCT ?uri ?firstName ?familyName ?bestuurseenheid ?bestuurseenheidPrefix WHERE {
          GRAPH <http://mu.semte.ch/graphs/public> {
            ?eenheid a besluit:Bestuurseenheid;
                      mu:uuid ?bestuurseenheidUUID.
            OPTIONAL {
              ?eenheid skos:prefLabel ?bestuurseenheid.
            }
            OPTIONAL {
              ?eenheid besluit:classificatie ?classificatie.
              ?classificatie skos:prefLabel ?bestuurseenheidPrefix.
            }
          }
          BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?bestuurseenheidUUID)) as ?graph)
      
          GRAPH ?graph {
            ?uri a foaf:Person;
              foaf:account ?accountURI.
            ?accountURI foaf:accountServiceHomepage ?provider.
            OPTIONAL {
              ?uri foaf:firstName ?firstName.
            }
            OPTIONAL {
              ?uri foaf:familyName ?familyName.
            }
            FILTER(?provider != <https://github.com/lblod/mock-login-service>)
          }
        }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((account) => {
      return {
        uri: account.uri.value,
        firstName: account.firstName ? account.firstName.value : '',
        familyName: account.familyName ? account.familyName.value : '',
        bestuurseenheid: (account.bestuurseenheidPrefix && account.bestuurseenheid) ? `${account.bestuurseenheidPrefix.value} ${account.bestuurseenheid.value}` : '',
      };
    });

    await generateReportFromData(data, ['uri', 'firstName', 'familyName', 'bestuurseenheid'], reportData);
  }
};