import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 0 * * *',
  name: 'emailsInFailboxWithin24Hours',
  execute: async () => {
    const reportData = {
      title: 'Lijst alle e-mails in failbox in de afgelopen 24 uur',
      description: 'Lists all emails in failbox within previous 24 hours',
      filePrefix: 'emails-in-failbox-within-24-hours'
    };
    console.log('Generate list of unsent e-mails within previous 24 hours report');
    const queryString = `
        PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    
        SELECT ?emailUri ?sentDate ?currentDate
        WHERE {
          ?emailUri nmo:isPartOf <http://data.lblod.info/id/mail-folders/6> ;
                 nmo:sentDate ?sentDateString .
        
          BIND(NOW() AS ?now)
          BIND(?now - "P1D"^^xsd:duration AS ?yesterday)
          BIND(xsd:dateTime(?sentDateString) AS ?sentDate)
        
          FILTER(?sentDate >= ?yesterday && ?sentDate <= ?now)
        }
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => {
      return {
        emailUri: row.emailUri.value,
        sentDate: row.sentDate.value,
        currentDate: row.currentDate.value
      };
    });

    await generateReportFromData(data, ['emailUri', 'sentDate', 'currentDate'], reportData);
  }
};
