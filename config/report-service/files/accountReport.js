import {sparqlEscapeString } from 'mu';
import {generateReportFromData} from '../helpers.js'
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'accountsReport',
  execute: async () => {
    const reportData = {
      title: 'Accounts Report',
      description: 'All accounts and the bestuurseenheid they belong',
      filePrefix: 'accounts'
    }
    console.log('Generate Account Report')
    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        SELECT DISTINCT ?firstName ?familyName ?bestuurseenheid ?bestuurseenheidPrefix WHERE {
          GRAPH <http://mu.semte.ch/graphs/public> {
            ?eenheid a besluit:Bestuurseenheid;
                      mu:uuid ?bestuurseenheidUUID;
                      skos:prefLabel ?bestuurseenheid;
                      besluit:classificatie ?classificatie.
            ?classificatie skos:prefLabel ?bestuurseenheidPrefix.
          }
          BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?bestuurseenheidUUID)) as ?graph)
      
          GRAPH ?graph {
            ?uri a foaf:Person;
              foaf:firstName ?firstName;
              foaf:familyName ?familyName;
              foaf:account ?accountURI .
              ?accountURI foaf:accountServiceHomepage ?provider.
              FILTER(?provider != <https://github.com/lblod/mock-login-service>)
          }
        }
    `
    const queryResponse = await query(queryString)
    const data = queryResponse.results.bindings.map((account) => {
      
      return {
        firstName: account.firstName.value,
        familyName: account.familyName.value,
        bestuurseenheid: `${account.bestuurseenheidPrefix.value} ${account.bestuurseenheid.value}`,
      }
    })

    await generateReportFromData(data, ['firstName', 'familyName', 'bestuurseenheid'], reportData)
  }
}