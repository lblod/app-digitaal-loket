import * as rst from 'rdf-string-ttl';
import * as uti from './utils';
import * as n3 from 'n3';
import { NAMESPACES as ns } from './namespaces';
const { literal } = n3.DataFactory;

export function recentMessageSubjectsAndDate(startDate) {
  const startDateLiteral = literal(startDate.toISOString(), ns.xsd`dateTime`);
  const alwaysFutureDateLiteral = literal(
    uti.addDays(new Date(), 1).toISOString(),
    ns.xsd`dateTime`,
  );

  return `
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sch:     <http://schema.org/>
PREFIX adms:    <http://www.w3.org/ns/adms#>
PREFIX deli:    <http://data.lblod.info/id/status/berichtencentrum/sync-with-kalliope/delivered/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX core:    <http://mu.semte.ch/vocabularies/core/>

CONSTRUCT {
  ?message
    rdf:type sch:Message ;
    sch:dateSent ?dateSent ;
    <ingraph> ?organisationGraph .
}
WHERE {
  GRAPH <http://mu.semte.ch/graphs/public> {
    ?organisation
      rdf:type besluit:Bestuurseenheid ;
      core:uuid ?organisationUUID .
  }
  BIND (IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", STR(?organisationUUID), "/LoketLB-berichtenGebruiker")) AS ?organisationGraph)
  GRAPH ?organisationGraph {
    ?message rdf:type sch:Message .
    FILTER NOT EXISTS {
      ?message adms:status deli:unconfirmed .
    }
    FILTER NOT EXISTS {
      ?message adms:status deli:failedConfirmation .
    }
    OPTIONAL { ?message sch:dateSent ?dateSent . }
    BIND (
      COALESCE(?dateSent, ${rst.termToString(alwaysFutureDateLiteral)})
      AS ?compareDate)
    FILTER (?compareDate > ${rst.termToString(startDateLiteral)})
  }
}`;
}

export function getMessageProperties(organisationGraph, message) {
  return `
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sch:     <http://schema.org/>
PREFIX nie:     <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
PREFIX nfo:     <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX adms:    <http://www.w3.org/ns/adms#>
PREFIX deli:    <http://data.lblod.info/id/status/berichtencentrum/sync-with-kalliope/delivered/>
PREFIX js:      <http://redpencil.data.gift/id/concept/JobStatus/>
PREFIX task:    <http://redpencil.data.gift/vocabularies/tasks/>
PREFIX jo:      <http://lblod.data.gift/id/jobs/concept/JobOperation/>
PREFIX dct:     <http://purl.org/dc/terms/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX account: <http://mu.semte.ch/vocabularies/account/>
PREFIX core:    <http://mu.semte.ch/vocabularies/core/>
PREFIX cogs:    <http://vocab.deri.ie/cogs#>
PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>

CONSTRUCT {
  ?conversation
    a sch:Conversation ;
    sch:hasPart ?message ;
    sch:about ?about ;
    sch:identifier ?identifier .
  ?message
    dct:type ?type ;
    sch:dateReceived ?dateReceived ;
    sch:text ?content ;
    sch:sender ?sender ;
    sch:recipient ?recipient ;
    adms:status ?confirmedStatus ;
    nie:hasPart ?attachment .
  ?attachment nfo:fileName ?attachmentFileName .
  ?job
    dct:subject ?message ;
    task:operation jo:harvestBericht ;
    dct:creator ?creator ;
    a cogs:Job .
  ?sender 
    skos:prefLabel ?sendername ;
    besluit:classificatie ?senderClassificatieCode .
  ?recipient
    skos:prefLabel ?recipientname ;
    besluit:classificatie ?recipientClassificatieCode .
  ?senderClassificatieCode skos:prefLabel ?senderTypeBestuur .
  ?recipientClassificatieCode skos:prefLabel ?recipientTypeBestuur .
}
WHERE {
  VALUES ?message { ${rst.termToString(message)} }
  GRAPH ${rst.termToString(organisationGraph)} {
    {
      ?conversation
        a sch:Conversation ;
        sch:hasPart ?message .
      OPTIONAL { ?conversation sch:about ?about . }
      OPTIONAL { ?conversation sch:identifier ?identifier . }
    } UNION {
      ?message dct:type ?type .
    } UNION {
      ?message sch:dateReceived ?dateReceived .
    } UNION {
      ?message sch:text ?content .
    } UNION {
      ?message sch:sender ?sender .
    } UNION {
      ?message sch:recipient ?recipient .
    } UNION {
      ?message adms:status ?confirmedStatus .
    } UNION {
      ?job
        dct:subject ?message ;
        task:operation jo:harvestBericht ;
        dct:creator ?creator ;
        a cogs:Job .
    } UNION {
      ?message nie:hasPart ?attachment .
      ?attachment
        rdf:type nfo:FileDataObject ;
        nfo:fileName ?attachmentFileName .
    }
  }
  OPTIONAL {
    ?message sch:sender ?sender .
    ?sender skos:prefLabel ?sendername .
    OPTIONAL {
      ?sender besluit:classificatie ?senderClassificatieCode .
      ?senderClassificatieCode skos:prefLabel ?senderTypeBestuur .
    }
  }
  OPTIONAL {
    ?message sch:recipient ?recipient .
    ?recipient skos:prefLabel ?recipientname .
    OPTIONAL {
      ?recipient besluit:classificatie ?recipientClassificatieCode .
      ?recipientClassificatieCode skos:prefLabel ?recipientTypeBestuur .
    }
  }
}`;
}
