import * as queries from './queries';
import * as hel from '../../helpers';
import * as rst from 'rdf-string-ttl';
import * as uti from './utils';
import * as n3 from 'n3';
import { SparqlJsonParser } from 'sparqljson-parse';
const { namedNode } = n3.DataFactory;
const sparqlJsonParser = new SparqlJsonParser();

const SERVICE_KALLIOPE =
  'https://github.com/lblod/berichtencentrum-sync-with-kalliope-service';
const SERVICE_LOKET = 'https://github.com/lblod/frontend-loket';

async function generate() {
  const startDate = uti.addDays(new Date(), -6 * 31); //6 months ago
  const messageQuery = queries.recentMessages(startDate);
  const messageResults = await hel.batchedQuery(messageQuery, 1000);
  const messages = sparqlJsonParser.parseJsonResults(messageResults);

  for (const entry of messages) {
    entry.provenance = entry?.creator;
    entry.provenance =
      entry?.provenance ||
      (entry?.confirmedStatus ? namedNode(SERVICE_KALLIOPE) : undefined);
    entry.provenance = entry?.provenance || namedNode(SERVICE_LOKET);

    entry.conversation = entry?.conversation?.value;
    entry.message = entry?.message?.value;
    entry.sender = entry?.sender?.value;
    entry.recipient = entry?.recipient?.value;
    entry.type = entry?.type?.value;
    entry.dateSent = entry?.dateSent?.value;
    entry.dateReceived = entry?.dateReceived?.value;
    entry.content = rst.termToString(entry.content);
    entry.attachment = entry?.attachment?.value;
    entry.provenance = entry?.provenance?.value;
  }

  await hel.generateReportFromData(
    messages,
    [
      'conversation',
      'message',
      'sender',
      'recipient',
      'type',
      'dateSent',
      'dateReceived',
      'content',
      'attachment',
      'provenance',
    ],
    {
      title: 'Message report with provenance',
      description:
        'Report about the Conversations and Messages with where they came from in the past 6 months.',
      filePrefix: 'messages-with-provenance-6-months',
    }
  );
}

export default {
  cronPattern: '0 4 * * *',
  name: 'messages-with-provenance',
  execute: generate,
};
