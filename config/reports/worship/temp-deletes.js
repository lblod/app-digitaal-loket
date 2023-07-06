import * as mas from '@lblod/mu-auth-sudo';
import * as utils from './utils';
import * as queries from './queries';
import { generateReportFromData } from '../../helpers';

const TEMP_DELETES_GRAPH =
  'http://eredienst-mandatarissen-consumer/temp-deletes';

async function generate() {
  let collectedData =
    (await mas.querySudo(queries.allFromGraph(TEMP_DELETES_GRAPH)))?.results
      ?.bindings || [];
  collectedData = collectedData.map((res) => {
    return {
      subject: utils.formatTerm(res.s),
      predicate: utils.formatTerm(res.p),
      object: utils.formatTerm(res.o),
    };
  });
  await generateReportFromData(
    collectedData,
    ['subject', 'predicate', 'object'],
    {
      title: 'Eredienst Temporary Deletes Report',
      description:
        'All eredienst triples ready for deletion from organisations.',
      filePrefix: 'eredienst-temp-deletes',
    }
  );
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-temp-deletes',
  execute: generate,
};
