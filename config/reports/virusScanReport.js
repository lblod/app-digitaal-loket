import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '4 2 * * *',
  name: 'virusScanReport',
  execute: async () => {
    const reportData = {
      title: 'Virus Scan Report',
      description:
        'A list of files that are considered malicious, or where the status is unknown',
      filePrefix: 'virus-scan-report',
    };

    const queryString = `
      PREFIX stix: <http://docs.oasis-open.org/cti/ns/stix#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
      PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>

      SELECT DISTINCT ?file ?fileName ?scanResult ?scanDate WHERE {
        GRAPH ?g {
          ?analysis a stix:MalwareAnalysis ;
            stix:analysis_started ?scanDate ;
            stix:sample_ref ?file ;
            stix:result ?scanResult .
            FILTER (?scanResult IN ("malicious", "unknown"))

            ?file nfo:fileName ?fileName .
        }
      }
      ORDER BY ?scanResult DESC(?scanDate)
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((data) => ({
      file: data.file.value,
      fileName: data.fileName.value,
      scanResult: data.scanResult.value,
      scanDate: data.scanDate.value,
    }));

    await generateReportFromData(
      data,
      ['file', 'fileName', 'scanResult', 'scanDate'],
      reportData,
    );
  },
};
