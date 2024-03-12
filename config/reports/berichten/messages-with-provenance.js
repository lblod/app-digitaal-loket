import * as queries from './queries';
import * as hel from '../../helpers';
import * as rst from 'rdf-string-ttl';
import * as uti from './utils';
import * as mas from '@lblod/mu-auth-sudo';
import { NAMESPACES as ns } from './namespaces';
import * as n3 from 'n3';
import { SparqlJsonParser } from 'sparqljson-parse';
const { namedNode } = n3.DataFactory;
const sparqlJsonParser = new SparqlJsonParser();

const SERVICE_KALLIOPE =
  'https://github.com/lblod/berichtencentrum-sync-with-kalliope-service';
const SERVICE_LOKET = 'https://github.com/lblod/frontend-loket';

async function generate() {
  const startDate = uti.addDays(new Date(), -6 * 31); //6 months ago
  const messagesSubjectsQuery = queries.recentMessageSubjectsAndDate(startDate);
  const response = await mas.querySudo(messagesSubjectsQuery);
  const parsedResults = sparqlJsonParser.parseJsonResults(response);
  const store = new n3.Store();
  parsedResults.forEach((t) => store.addQuad(t.s, t.p, t.o));
  const resultMessages = [];
  
  for (const message of store.getSubjects()) {
    const organisationGraph = store.getObjects(message, namedNode('ingraph'))[0];
    const detailsQuery = queries.getMessageProperties(organisationGraph, message);
    const detailsResponse = await mas.querySudo(detailsQuery);
    const detailsParsedResults = sparqlJsonParser.parseJsonResults(detailsResponse);
    const detailsStore = new n3.Store();
    detailsParsedResults.forEach((t) => detailsStore.addQuad(t.s, t.p, t.o));
    
    const recipient = detailsStore.getObjects(message, ns.sch`recipient`)[0];
    const sender = detailsStore.getObjects(message, ns.sch`sender`)[0];
    const confirmedStatus = detailsStore.getObjects(message, ns.adms`status`)[0];
    let job = detailsStore.getSubjects(ns.dct`subject`, message)[0];
    job = detailsStore.has(job, ns.rdf`type`, ns.cogs`Job`) ? job : undefined;
    let provenance = detailsStore.getObjects(job, ns.dct`creator`)[0];
    provenance = provenance || (confirmedStatus ? namedNode(SERVICE_KALLIOPE) : undefined);
    provenance = provenance || namedNode(SERVICE_LOKET);
    const attachments = detailsStore.getObjects(message, ns.nie`hasPart`);
    const filenames = attachments.map((att) => detailsStore.getObjects(att, ns.nfo`fileName`)[0]);
    const filenamesFormatted = `[${filenames.length}]:${filenames.map((f) => f.value).join(',')}`;
    const conversation = detailsStore.getSubjects(ns.sch`hasPart`, message)[0];

    resultMessages.push({
      conversation: conversation?.value,
      identifier: detailsStore.getObjects(conversation, ns.sch`identifier`)[0]?.value,
      about: detailsStore.getObjects(conversation, ns.sch`about`)[0]?.value,
      message: message.value,
      dateSent: store.getObjects(message, ns.sch`dateSent`)[0]?.value,
      type: detailsStore.getObjects(message, ns.dct`type`)[0]?.value,
      dateReceived: detailsStore.getObjects(message, ns.sch`dateReceived`)[0]?.value,
      sender: sender?.value,
      recipient: recipient?.value,
      sendername: detailsStore.getObjects(sender, ns.skos`prefLabel`)[0]?.value,
      recipientname: detailsStore.getObjects(recipient, ns.skos`prefLabel`)[0]?.value,
      attachments: filenamesFormatted,
      provenance: provenance?.value,
      content: detailsStore.getObjects(message, ns.sch`text`)[0]?.value,
    });
  }

  await hel.generateReportFromData(
    resultMessages,
    [
      'conversation',
      'identifier',
      'about',
      'message',
      'sender',
      'sendername',
      'recipient',
      'recipientname',
      'type',
      'dateSent',
      'dateReceived',
      'attachments',
      'provenance',
      'content',
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
