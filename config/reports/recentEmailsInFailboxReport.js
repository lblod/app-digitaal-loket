import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  // The report runs after midnight. This guarantees substracting one day
  // from the current date yields yesterday.
  cronPattern: '0 30 0 * * *',
  name: 'recentEmailsInFailbox',
  execute: async () => {
    const reportData = {
      title: 'Lijst alle e-mails in failbox van de vorige dag',
      description: 'Lists all emails in failbox from the previous day',
      filePrefix: 'recent-emails-in-failbox'
    };
    console.log('Generate list of unsent e-mails within previous 24 hours report');

    // <http://data.lblod.info/id/mail-folders/6> is the fail box.
    //
    // The sent date in emails contains the "Z" suffix for the timezone, which
    // was affecting the comparison with other date times which do not have a "Z" timezone suffix.
    const queryString = `
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

      SELECT ?emailUri ?messageSubject ?date
      WHERE {
        ?emailUri nmo:isPartOf <http://data.lblod.info/id/mail-folders/6> ;
          nmo:messageSubject ?messageSubject ;
          nmo:sentDate ?sentDate .

        BIND(xsd:dateTime(REPLACE(STR(?sentDate), "Z", "")) AS ?date)
        BIND(xsd:date(NOW() - "P1D"^^xsd:duration) AS ?yesterday)

        FILTER(xsd:date(?date) = ?yesterday)
      } ORDER BY DESC(?date)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((row) => {
      return {
        emailUri: row.emailUri.value,
        messageSubject: row.messageSubject.value,
        date: row.date.value
      };
    });

    await generateReportFromData(data, ['emailUri', 'messageSubject', 'date'], reportData);
  }
};
