import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 6 * * *',
  name: 'lpdcConceptsReport',
  execute: async () =>{
    const reportData = {
      title: 'Overview LPDC concepts used so far',
      description: 'Overview LPDC concepts used so far',
      filePrefix: 'lpdcConceptsReport'
    };

    console.log('Generating LPDC reports for concept used');
    const queryString  = `
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX lpdcExt: <https://productencatalogus.data.vlaanderen.be/ns/ipdc-lpdc#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX cpsv: <http://purl.org/vocab/cpsv#>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX pav: <http://purl.org/pav/>

      SELECT DISTINCT ?uriPublicService ?uriBestuurseenheid ?naam ?concept ?status ?statusLabel  WHERE {

            ?uriPublicService a cpsv:PublicService;
                  adms:status ?status;
                  dct:title ?title;
                  pav:createdBy ?uriBestuurseenheid.

            ?uriBestuurseenheid a besluit:Bestuurseenheid;
                  skos:prefLabel ?naam;
                  besluit:classificatie ?typeUri.

            ?status skos:prefLabel ?statusLabel.

            OPTIONAL { ?uriPublicService dct:source ?source.}

      }`;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings;

    const postProcessedData = data.map(r => ({
      uriPublicService: r.uriPublicService.value,
      uriBestuurseenheid: r.uriBestuurseenheid.value,
      naam: r.naam.value,
      statusLabel: r.statusLabel.value,
      status: r.status.value,
      concept: (r.concept)?r.concept.value:"", //use empty string for non-existing concepts
    }));
    const csvHeaders = Object.keys(postProcessedData[0]);
    await generateReportFromData(postProcessedData, csvHeaders, reportData);
  }
};
