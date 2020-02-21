import {generateReportFromData} from '../helpers.js'
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 * * *',
  name: 'berichtencentrumMessagesReport',
  execute: async () => {
    const reportData = {
      title: 'Berichtencentrum Messages Report',
      description: 'All new messages in Berichtencentrum',
      filePrefix: 'berichtencentrumMessages'
    }
    console.log('Generate Berichtencentrum Messages Report')
    const queryString = `
      PREFIX schema: <http://schema.org/>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      
      select ?datesent ?betreft ?dossiernr ?bestuurNaam ?bestuur  WHERE {
              ?conversation a schema:Conversation;
              schema:hasPart ?s;
              schema:about ?betreft;
              schema:identifier ?dossiernr.
              ?s a <http://schema.org/Message>;
              schema:sender ?bestuur;
              schema:dateSent ?datesent.
            FILTER(?bestuur != <http://data.lblod.info/id/bestuurseenheden/141d9d6b-54af-4d17-b313-8d1c30bc3f5b>)
            FILTER NOT EXISTS {
              ?sNext a <http://schema.org/Message>;
                          schema:sender <http://data.lblod.info/id/bestuurseenheden/141d9d6b-54af-4d17-b313-8d1c30bc3f5b> ;
                          schema:dateSent ?nextDatesent .
            ?conversation schema:hasPart ?sNext .
            FILTER (?s != ?sNext)
            FILTER (?nextDatesent > ?datesent)
          }
          GRAPH <http://mu.semte.ch/graphs/public> {
            ?bestuur skos:prefLabel ?naam;
            besluit:classificatie/skos:prefLabel ?bestuurType.
            BIND(CONCAT(?bestuurType, " ", ?naam) AS ?bestuurNaam)
          }
          } ORDER BY DESC(?datesent)
    `
    const queryResponse = await query(queryString)
    const data = queryResponse.results.bindings.map((row) => ({
      datesent: row.datesent.value,
      betreft: row.betreft.value,
      dossiernr: row.dossiernr.value,
      bestuurNaam: row.bestuurNaam.value,
      bestuur: row.bestuur.value,
    }))
    await generateReportFromData(data, ['datesent', 'betreft', 'dossiernr', 'bestuurNaam', 'bestuur'], reportData)
  }
}
