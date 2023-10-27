const HOSTNAME = 'http://localhost';

export const subjects = [
  //For Submissions via Automatic Submission Flow
  //TODO could use some improvements (maybe add some more things like a Job)
  {
    type: 'http://rdf.myexperiment.org/ontologies/base/Submission',
    trigger: `
      ?subject a <http://rdf.myexperiment.org/ontologies/base/Submission> .
    `,
    path: `
      ?subject
        pav:createdBy ?organisation ;
        pav:providedBy ?vendor .
    `,
    remove: {
      delete: `
        ?subject ?p ?o .
      `,
      where: `
        ?subject ?p ?o .
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
      `,
      where: `
        ?subject ?p ?o .
      `,
    },
  },
  //For the Berichten Melding via berichten-melding-service
  //Failed jobs
  {
    type: 'http://vocab.deri.ie/cogs#Job',
    trigger: `
      ?subject
        <http://www.w3.org/ns/adms#status>
          <http://redpencil.data.gift/id/concept/JobStatus/failed> ;
        <http://redpencil.data.gift/vocabularies/tasks/operation>
          <http://lblod.data.gift/id/jobs/concept/JobOperation/harvestBericht> .
    `,
    path: `
      ?subject
        <http://purl.org/pav/providedBy> ?vendor ;
        <http://schema.org/sender> ?organisation .
    `,
      copy: {
         insert: `
           ?subject ?p ?o.
           ?error ?errorP ?errorO.`,
         where: `
             ?subject ?p ?o .
             OPTIONAL {
                ?subject <http://redpencil.data.gift/vocabularies/tasks/error> ?error.
                ?error ?errorP ?errorO.
             }
          `
      },
      remove: {
         delete: `
           ?subject ?p ?o.
           ?error ?errorP ?errorO.`,
         where: `
             ?subject ?p ?o .
             OPTIONAL {
                ?subject <http://redpencil.data.gift/vocabularies/tasks/error> ?error.
                ?error ?errorP ?errorO.
             }
          `
      }
  },
  //Success jobs
  {
    type: 'http://vocab.deri.ie/cogs#Job',
    trigger: `
      ?subject
        <http://www.w3.org/ns/adms#status>
          <http://redpencil.data.gift/id/concept/JobStatus/success> ;
        <http://redpencil.data.gift/vocabularies/tasks/operation>
          <http://lblod.data.gift/id/jobs/concept/JobOperation/harvestBericht> .
    `,
    path: `
      ?subject
        <http://purl.org/pav/providedBy> ?vendor ;
        <http://schema.org/sender> ?organisation .
    `,
    remove: {
      delete: `
        ?conversatie ?pc ?oc .
      `,
      where: `
        GRAPH ?g {
          ?subject
            <http://purl.org/dc/terms/subject> ?bericht .
          ?bericht
            a <http://schema.org/Message> .
          ?conversatie
            <http://schema.org/hasPart> ?bericht ;
            a <http://schema.org/Conversation> .
          ?conversatie ?pc ?oc .
        }
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
        ?conversatie ?pc ?co .
        ?bericht ?pa ?ca .
        ?bijlage ?pb ?ob .
        ?physicalBijlage ?pp ?op .

        ?bijlage
          <http://mu.semte.ch/vocabularies/ext/downloadLink>
            ?bijlageDownloadLink .
      `,
      where: `
        GRAPH ?g {
          ?subject
            <http://purl.org/dc/terms/subject> ?bericht .
          ?bericht
            a <http://schema.org/Message> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#hasPart>
              ?bijlage .
          ?conversatie
            <http://schema.org/hasPart> ?bericht ;
            a <http://schema.org/Conversation> .
          ?bijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://mu.semte.ch/vocabularies/core/uuid> ?bijlageUUID .
          ?physicalBijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#dataSource>
              ?bijlage .

          BIND (CONCAT("${HOSTNAME}/files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

          ?subject ?p ?o .
          ?conversatie ?pc ?co .
          ?bericht ?pa ?ca .
          ?bijlage ?pb ?ob .
          ?physicalBijlage ?pp ?op .
        }
      `,
    },
  },
  //For Berichten en Conversaties synced from Kaliope
  {
    type: 'http://schema.org/Message',
    trigger: `
      ?subject
        <http://www.w3.org/ns/adms#status>
          <http://data.lblod.info/id/status/berichtencentrum/sync-with-kalliope/delivered/confirmed> .
    `,
    path: `
      GRAPH ?h {
        ?organisation a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid> .
      }
      GRAPH <http://mu.semte.ch/graphs/automatic-submission> {
        ?vendor <http://mu.semte.ch/vocabularies/account/canActOnBehalfOf> ?organisation .
      }
      GRAPH ?g {
        ?subject ?a ?b .
        ?organisation ?p ?o .
      }
    `,
    remove: {
      delete: `
        ?conversatie ?pc ?oc .
      `,
      where: `
        GRAPH ?g {
          ?conversatie
            <http://schema.org/hasPart> ?subject ;
            a <http://schema.org/Conversation> .
        }
        ?conversatie ?pc ?oc .
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
        ?conversatie ?pc ?co .
        ?bijlage ?pb ?ob .
        ?physicalBijlage ?pp ?op .

        ?bijlage
          <http://mu.semte.ch/vocabularies/ext/downloadLink>
            ?bijlageDownloadLink .
      `,
      where: `
        GRAPH ?g {
          ?conversatie
            <http://schema.org/hasPart> ?subject ;
            a <http://schema.org/Conversation> .
          ?subject
            a <http://schema.org/Message> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#hasPart>
              ?bijlage .
          ?bijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://mu.semte.ch/vocabularies/core/uuid> ?bijlageUUID .
          ?physicalBijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#dataSource>
              ?bijlage .

          BIND (CONCAT("${HOSTNAME}/files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

          ?subject ?p ?o .
          ?conversatie ?pc ?co .
          ?bijlage ?pb ?ob .
          ?physicalBijlage ?pp ?op .
        }
      `,
    }
  },
  //For Berichten saved in Loket
  //TODO: Trigger is not yet working like this
  {
    type: 'http://schema.org/Message',
    trigger: `
      ?subject
        <http://mu.semte.ch/vocabularies/ext/creator>
          "https://github.com/lblod/frontend-loket/" .
    `,
    path: `
      GRAPH ?h {
        ?organisation a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid> .
      }
      GRAPH <http://mu.semte.ch/graphs/automatic-submission> {
        ?vendor <http://mu.semte.ch/vocabularies/account/canActOnBehalfOf> ?organisation .
      }
      GRAPH ?g {
        ?subject ?a ?b .
        ?organisation ?p ?o .
      }
    `,
    remove: {
      delete: `
        ?conversatie ?pc ?oc .
      `,
      where: `
        GRAPH ?g {
          ?conversatie
            <http://schema.org/hasPart> ?subject ;
            a <http://schema.org/Conversation> .
        }
        ?conversatie ?pc ?oc .
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
        ?conversatie ?pc ?co .
        ?bijlage ?pb ?ob .
        ?physicalBijlage ?pp ?op .

        ?bijlage
          <http://mu.semte.ch/vocabularies/ext/downloadLink>
            ?bijlageDownloadLink .
      `,
      where: `
        GRAPH ?g {
          ?conversatie
            <http://schema.org/hasPart> ?subject ;
            a <http://schema.org/Conversation> .
          ?subject
            a <http://schema.org/Message> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#hasPart>
              ?bijlage .
          ?bijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://mu.semte.ch/vocabularies/core/uuid> ?bijlageUUID .
          ?physicalBijlage
            a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject> ;
            <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#dataSource>
              ?bijlage .

          BIND (CONCAT("${HOSTNAME}/files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

          ?subject ?p ?o .
          ?conversatie ?pc ?co .
          ?bijlage ?pb ?ob .
          ?physicalBijlage ?pp ?op .
        }
      `,
    }
  },
];
