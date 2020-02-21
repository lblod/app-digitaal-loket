import {query} from 'mu';
import {generateReportFromData} from '../helpers.js'

export default {
  cronPattern: '0 0 * * *',
  name: 'bestuurseenhedenReport',
  execute: async () => {
    const reportData = {
      title: 'Bestuurseenheden Report',
      description: 'All Bestuurseenheden',
      filePrefix: 'bestuurseenheden'
    }
    console.log('Generate Bestuurseenheden Report')
    const queryString = `
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      
      select distinct ?name ?type ?province ?uri where {
        ?uri a besluit:Bestuurseenheid;
          skos:prefLabel ?name;
          ext:inProvincie ?provinceURI;
          besluit:classificatie ?typeURI .
        ?provinceURI rdfs:label ?province.
        ?typeURI skos:prefLabel ?type .
      }
    `
    const queryResponse = await query(queryString)
    const data = queryResponse.results.bindings.map((bestuurseenheid) => ({
      name: bestuurseenheid.name.value,
      type: bestuurseenheid.type.value,
      province: bestuurseenheid.province.value,
      uri: bestuurseenheid.uri.value,
    }))
    await generateReportFromData(data, ['name', 'type', 'province', 'uri'], reportData)
  }
}
