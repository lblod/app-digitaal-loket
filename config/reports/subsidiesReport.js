import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 23 * * 6',
  name: 'subsidiesReport',
  execute: async () => {
    const reportData = {
      title: 'Lijst van subsidies',
      description: 'All subsidies with their related information',
      filePrefix: 'subsidiesReport'
    };
    console.log('Generate Subsidies Report');
    const queryString = `
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX schema: <http://schema.org/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX pav: <http://purl.org/pav/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX gleif: <https://www.gleif.org/ontology/Base/>

      SELECT DISTINCT ?aanvraagdatum ?bedrag (?bestuurseenheidLabel as ?bestuurseenheid)
      (?aanbodLabel as ?aanbod) (?statusLabel as ?status) (?reekLabel as ?reek) ?reekStart ?reekEnd
      ?createdByName ?modifiedByName ?subsidie
      WHERE {
        ?subsidie a lblodSubsidie:ApplicationForm ;
          subsidie:aanvraagdatum ?aanvraagdatum ;
          pav:createdBy ?bestuurseenheid ;
          dct:creator ?createdBy ;
          ext:lastModifiedBy ?modifiedBy ;
          adms:status ?status .

        OPTIONAL {
          ?subsidie lblodSubsidie:totalAmount ?amount .
        }
        BIND(IF(BOUND(?amount), ?amount, xsd:float(0)) as ?bedrag)

        ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel .

        OPTIONAL {
          ?subsidie lblodSubsidie:timeBlock ?reek .
          ?reek skos:prefLabel ?reekLabel ;
            gleif:hasStart ?reekStart ;
            gleif:hasEnd ?reekEnd ;
            ext:submissionPeriod ?submissionPeriod .
        }

        OPTIONAL {
          ?subsidie lblodSubsidie:subsidyMeasure ?aanbod .
          ?aanbod skos:prefLabel ?aanbodLabel .
        }

        ?createdBy foaf:firstName ?createdByFirstName ;
          foaf:familyName ?createdByLastName .

        BIND(CONCAT(?createdByFirstName, " ", ?createdByLastName) as ?createdByName)

        ?modifiedBy foaf:firstName ?modifiedByFirstName ;
          foaf:familyName ?modifiedByLastName .

        BIND(CONCAT(?modifiedByFirstName, " ", ?modifiedByLastName) as ?modifiedByName)

        ?status skos:prefLabel ?statusLabel .
      }
      ORDER BY DESC(?aanvraagdatum)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        aanvraagdatum: subsidie.aanvraagdatum.value,
        bedrag: subsidie.bedrag.value,
        bestuurseenheid: subsidie.bestuurseenheid.value,
        aanbod: subsidie.aanbod ? subsidie.aanbod.value : '',
        status: subsidie.status.value,
        reek: subsidie.reek ? subsidie.reek.value : '',
        reekStart: subsidie.reekStart ? subsidie.reekStart.value : '',
        reekEnd: subsidie.reekEnd ? subsidie.reekEnd.value : '',
        createdByName: subsidie.createdByName.value,
        modifiedByName: subsidie.modifiedByName.value,
        subsidie: subsidie.subsidie.value
      };
    });

     await generateReportFromData(data, [
       'aanvraagdatum',
       'bedrag',
       'bestuurseenheid',
       'aanbod',
       'status',
       'reek',
       'reekStart',
       'reekEnd',
       'createdByName',
       'modifiedByName',
       'subsidie'
      ], reportData);
  }
};
