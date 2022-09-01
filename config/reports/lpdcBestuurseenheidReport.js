import {generateReportFromData} from '../helpers.js'
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '',
  name: 'lpdcBestuursenheid',
  execute: async () =>{
    const reportData = {
      title: 'LPDC bestuursenheid report',
      description: 'LPDC bestuursenheid report',
      filePrefix: 'lpdcBestuursenheid'
    };
    console.log('Generating LPDC Bestuursenheid Report');
    const queryString  = `
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>  
      PREFIX lpdcExt: <https://productencatalogus.data.vlaanderen.be/ns/ipdc-lpdc#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX cpsv: <http://purl.org/vocab/cpsv#>
      SELECT ?uriBestuurseenheid ?naam ?uriPublicService ?title ?modified ?status WHERE {
            ?uriBestuurseenheid a besluit:Bestuurseenheid;
                  skos:prefLabel ?naam.

            ?uriPublicService a cpsv:PublicService;
                  adms:status ?status;
                  dct:modified ?modified;
                  dct:title ?title.
        }
    `;
    const queryResponse = await query(queryString);
    const bindings = queryResponse.results.bindings;
    await generateReportFromData(data, ['lpdcBestuursenheid'], reportData);
  }
}
