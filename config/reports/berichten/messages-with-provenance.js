import * as queries from './queries';
import * as hel from '../../helpers';
import * as uti from './utils';
import * as mas from '@lblod/mu-auth-sudo';
import { NAMESPACES as ns } from './namespaces';
import * as n3 from 'n3';
import { SparqlJsonParser } from 'sparqljson-parse';
const { namedNode } = n3.DataFactory;
const sparqlJsonParser = new SparqlJsonParser();

const SERVICE_KALLIOPE = 'https://github.com/lblod/berichtencentrum-sync-with-kalliope-service';
const SERVICE_LOKET = 'https://github.com/lblod/frontend-loket';

async function generate() {
  const startDate = uti.addDays(new Date(), -12 * 31); //12 month ago
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

    const conversation = detailsStore.getSubjects(ns.sch`hasPart`, message)[0];
    const conversationV = conversation?.value;
    const identifierV = detailsStore.getObjects(conversation, ns.sch`identifier`)[0]?.value;
    const aboutV = detailsStore.getObjects(conversation, ns.sch`about`)[0]?.value;
    const messageV = message.value;
    const dateSentV = store.getObjects(message, ns.sch`dateSent`)[0]?.value;
    const typeV = detailsStore.getObjects(message, ns.dct`type`)[0]?.value;
    const dateReceivedV = detailsStore.getObjects(message, ns.sch`dateReceived`)[0]?.value;
    const sender = detailsStore.getObjects(message, ns.sch`sender`)[0];
    const senderV = sender?.value;
    const sendernameV = detailsStore.getObjects(sender, ns.skos`prefLabel`)[0]?.value;
    const senderClassificatieCode = detailsStore.getObjects(sender, ns.besluit`classificatie`)[0];
    let senderTypeBestuurV;
    if (senderClassificatieCode) {
      const senderTypeBestuur = detailsStore.getObjects(senderClassificatieCode, ns.skos`prefLabel`)[0];
      senderTypeBestuurV = senderTypeBestuur?.value;
    }
    const recipient = detailsStore.getObjects(message, ns.sch`recipient`)[0];
    const recipientV = recipient?.value;
    const recipientnameV = detailsStore.getObjects(recipient, ns.skos`prefLabel`)[0]?.value;
    const recipientClassificatieCode = detailsStore.getObjects(recipient, ns.besluit`classificatie`)[0];
    let recipientTypeBestuurV;
    if (recipientClassificatieCode) {
      const recipientTypeBestuur = detailsStore.getObjects(recipientClassificatieCode, ns.skos`prefLabel`)[0];
      recipientTypeBestuurV = recipientTypeBestuur?.value;
    }
    const confirmedStatus = detailsStore.getObjects(message, ns.adms`status`)[0];
    let job = detailsStore.getSubjects(ns.dct`subject`, message)[0];
    job = detailsStore.has(job, ns.rdf`type`, ns.cogs`Job`) ? job : undefined;
    let provenance = detailsStore.getObjects(job, ns.dct`creator`)[0];
    provenance = provenance || (confirmedStatus ? namedNode(SERVICE_KALLIOPE) : undefined);
    provenance = provenance || namedNode(SERVICE_LOKET);
    const provenanceV = provenance?.value;
    const contentV = detailsStore.getObjects(message, ns.sch`text`)[0]?.value;

    const attachments = detailsStore.getObjects(message, ns.nie`hasPart`);

    if (attachments.length === 0) {
      resultMessages.push({
        conversation: conversationV,
        identifier: identifierV,
        about: aboutV,
        message: messageV,
        dateSent: dateSentV,
        type: typeV,
        dateReceived: dateReceivedV,
        sender: senderV,
        recipient: recipientV,
        sendername: sendernameV,
        senderTypeBestuur: senderTypeBestuurV,
        recipientname: recipientnameV,
        recipientTypeBestuur: recipientTypeBestuurV,
        provenance: provenanceV,
        attachmentSequence: '0/0',
        filename: '',
        content: contentV,
      });
    }

    for (let i = 0; i < attachments.length; i++) {
      const attachmentSeqV = `${i + 1}/${attachments.length}`;
      const filenameV = detailsStore.getObjects(attachments[i], ns.nfo`fileName`)[0]?.value;

      resultMessages.push({
        conversation: conversationV,
        identifier: identifierV,
        about: aboutV,
        message: messageV,
        dateSent: dateSentV,
        type: typeV,
        dateReceived: dateReceivedV,
        sender: senderV,
        recipient: recipientV,
        sendername: sendernameV,
        senderTypeBestuur: senderTypeBestuurV,
        recipientname: recipientnameV,
        recipientTypeBestuur: recipientTypeBestuurV,
        provenance: provenanceV,
        attachmentSequence: attachmentSeqV,
        filename: filenameV,
        content: contentV,
      });
    }
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
      'senderTypeBestuur',
      'recipient',
      'recipientname',
      'recipientTypeBestuur',
      'type',
      'dateSent',
      'dateReceived',
      'provenance',
      'attachmentSequence',
      'filename',
      'content',
    ],
    {
      title: 'Message report with provenance',
      description: 'Report about the Conversations and Messages with where they came from in the past 12 months.',
      filePrefix: 'messages-with-provenance-12-months',
    },
  );
}

export default {
  cronPattern: '0 4 * * *',
  name: 'messages-with-provenance',
  execute: generate,
};
