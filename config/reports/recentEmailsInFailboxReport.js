import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 0 * * *',
  name: 'recentEmailsInFailbox',
  execute: async () => {
    const reportData = {
      title: 'Lijst alle e-mails in failbox in de afgelopen 24 uur',
      description: 'Lists all emails in failbox within previous 24 hours',
      filePrefix: 'recent-emails-in-failbox'
    };
    console.log('Generate list of unsent e-mails within previous 24 hours report');
    const queryString = `
        PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    
        SELECT ?emailUri ?messageSubject ?sentDate 
        WHERE {
          ?emailUri nmo:isPartOf <http://data.lblod.info/id/mail-folders/6> ;
                    nmo:messageSubject ?messageSubject ;
                    nmo:sentDate ?sentDateString .
        
          BIND(NOW() AS ?now)
          BIND(?now - "P1D"^^xsd:duration AS ?yesterday)
          BIND(xsd:dateTime(?sentDateString) AS ?sentDate)
        
          FILTER(?sentDate >= ?yesterday && ?sentDate <= ?now)
        } ORDER BY DESC(?sentDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => {
      return {
        emailUri: row.emailUri.value,
        messageSubject: row.messageSubject.value,
        sentDate: row.sentDate.value
      };
    });

    await generateReportFromData(data, ['emailUri', 'messageSubject', 'sentDate'], reportData);
  }
};
