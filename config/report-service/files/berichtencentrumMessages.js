import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'berichtencentrumMessagesReport',
  execute: async () => {
    const reportData = {
      title: 'Berichtencentrum Messages Report',
      description: 'All new messages in Berichtencentrum',
      filePrefix: 'berichtencentrumMessages'
    };
    console.log('Generate Berichtencentrum Messages Report');
    const queryString = `
      PREFIX schema: <http://schema.org/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      
      SELECT ?uri ?datesent ?betreft ?dossiernr ?bestuurNaam ?bestuur  WHERE {
        ?conversation a schema:Conversation;
          schema:hasPart ?uri.
        OPTIONAL {
          ?conversation schema:about ?betreft.
        }
        OPTIONAL {
          ?conversation schema:identifier ?dossiernr.
        }
        ?uri a <http://schema.org/Message>.
        OPTIONAL {
          ?uri schema:sender ?bestuur.
        }
        OPTIONAL {
          ?uri schema:dateSent ?datesent.
        }
        FILTER(?bestuur != <http://data.lblod.info/id/bestuurseenheden/141d9d6b-54af-4d17-b313-8d1c30bc3f5b>)
        FILTER NOT EXISTS {
            ?sNext a <http://schema.org/Message>;
              schema:sender <http://data.lblod.info/id/bestuurseenheden/141d9d6b-54af-4d17-b313-8d1c30bc3f5b> ;
              schema:dateSent ?nextDatesent .
            ?conversation schema:hasPart ?sNext .
          FILTER (?uri != ?sNext)
          FILTER (?nextDatesent > ?datesent)
        }
        GRAPH <http://mu.semte.ch/graphs/public> {
          OPTIONAL {
            ?bestuur skos:prefLabel ?naam;
              besluit:classificatie/skos:prefLabel ?bestuurType.
            BIND(CONCAT(?bestuurType, " ", ?naam) AS ?bestuurNaam)
          }
        }
      } ORDER BY DESC(?datesent)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => ({
      uri: row.uri ? row.uri.value : '',
      datesent: row.datesent ? row.datesent.value : '',
      betreft: row.betreft ? row.betreft.value : '',
      dossiernr: row.dossiernr ? row.dossiernr.value : '',
      bestuurNaam: row.bestuurNaam ? row.bestuurNaam.value : '',
      bestuur: row.bestuur ? row.bestuur.value : '',
    }));
    await generateReportFromData(data, ['uri', 'datesent', 'betreft', 'dossiernr', 'bestuurNaam', 'bestuur'], reportData);
  }
};
