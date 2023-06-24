import * as mas from '@lblod/mu-auth-sudo';
import * as utils from './utils';
import * as queries from './queries';
import { generateReportFromData } from '../../helpers';

const TEMP_INSERTS_GRAPH =
  'http://eredienst-mandatarissen-consumer/temp-inserts';

async function generate() {
  let collectedData =
    (await mas.querySudo(queries.allFromGraph(TEMP_INSERTS_GRAPH)))?.results
      ?.bindings || [];
  collectedData = collectedData.map((res) => {
    return {
      subject: `<${res.s.value}>`,
      predicate: `<${res.p.value}>`,
      object: utils.formatObjectTerm(res.o),
    };
  });
  await generateReportFromData(
    collectedData,
    ['subject', 'predicate', 'object'],
    {
      title: 'Eredienst Temporary Inserts Report',
      description:
        'All eredienst triples ready for dispatching to organisations.',
      filePrefix: 'eredienst-temp-inserts',
    }
  );
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-temp-inserts',
  execute: generate,
};
