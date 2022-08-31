import {generateReportFromData} from '../helpers.js'
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '',
  name: '',
  execute: async () =>{
    const reportData = {
    };
    const queryString  = `
    `;
    const queryResponse = await query(queryString);
    const bindings = queryResponse.results.bindings;
    await generateReportFromData(data, ['lpdcBestuursenheid'], reportData);
  }
}
