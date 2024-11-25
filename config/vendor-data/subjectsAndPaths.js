import envvar from 'env-var';

const HOSTNAME = envvar
  .get('HOSTNAME')
  .required()
  .example('http://localhost')
  .asUrlString();

export const subjects = [
  /////////////////////////////////////////////////////////////////////////////
  // Submissions
  // via Automatic Submission flow (not Loket)
  /////////////////////////////////////////////////////////////////////////////
  //
  // The following queries always copy the whole model at once. E.g. if there
  // is a Submission to be copied to the vendor graph, also copy the FormData,
  // SubmissionDocument, Artikel (for cross referencing), ... if they exist.
  // All data is also deleted before copied again. This is to make sure changes
  // are correctly updated. Otherwise you would see multiple values for the
  // same predicate because the old values are never deleted.
  // Note: Submissions via Loket are not copied to the vendor graphs. This is
  // because there is no vendor linked to the Submissions from Loket, and
  // vendors don't need to see Submission they haven't posted themselves. It is
  // possible to add them anyway, if needed.

  /**
   * Submission
   */

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
        ?submissionDocument ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?subject ?p ?o .
        } UNION {
          ?subject <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?subject <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject <http://www.w3.org/ns/prov#generated> ?formdata .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
        ?submissionDocument ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?subject ?p ?o .
        } UNION {
          ?subject <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?subject <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject <http://www.w3.org/ns/prov#generated> ?formdata .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
  },

  /**
   * SubmissionDocument
   */

  {
    type: 'http://mu.semte.ch/vocabularies/ext/SubmissionDocument',
    trigger: `
      ?subject a <http://mu.semte.ch/vocabularies/ext/SubmissionDocument> .
    `,
    path: `
      ?submission <http://purl.org/dc/terms/subject> ?subject .
      ?submission
        pav:createdBy ?organisation ;
        pav:providedBy ?vendor .
    `,
    remove: {
      delete: `
        ?submission ?sp ?so .
        ?subject ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://purl.org/dc/terms/subject> ?subject .
          ?submission ?p ?o .
        } UNION {
          ?subject ?p ?o .
        } UNION {
          ?submission <http://purl.org/dc/terms/subject> ?subject .
          ?submission <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
    copy: {
      insert: `
        ?submission ?sp ?so .
        ?subject ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://purl.org/dc/terms/subject> ?subject .
          ?submission ?sp ?so .
        } UNION {
          ?subject ?sdp ?sdo .
        } UNION {
          ?submission <http://purl.org/dc/terms/subject> ?subject .
          ?submission <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
  },

  /**
   * Artikel
   * This is a cross-referenced document
   */

  {
    type: 'http://data.vlaanderen.be/ns/besluit#Artikel',
    trigger: `
      ?subject a <http://data.vlaanderen.be/ns/besluit#Artikel> .
    `,
    path: `
      ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
      ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
      ?submission
        pav:createdBy ?organisation ;
        pav:providedBy ?vendor .
    `,
    remove: {
      delete: `
        ?submission ?p ?o .
        ?submissionDocument ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?subject ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submission ?p ?o .
        } UNION {
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submission <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject ?ap ?ao .
        }
      `,
    },
    copy: {
      insert: `
        ?submission ?p ?o .
        ?submissionDocument ?sdp ?sdo .
        ?formdata ?fp ?fo .
        ?subject ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submission ?p ?o .
        } UNION {
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?subject .
          ?submission <http://www.w3.org/ns/prov#generated> ?formdata .
          ?formdata ?fp ?fo .
        } UNION {
          ?subject ?ap ?ao .
        }
      `,
    },
  },

  /**
   * FormData
   */

  {
    type: 'http://lblod.data.gift/vocabularies/automatische-melding/FormData',
    trigger: `
      ?subject a <http://lblod.data.gift/vocabularies/automatische-melding/FormData> .
    `,
    path: `
      ?submission <http://www.w3.org/ns/prov#generated> ?subject .
      ?submission
        pav:createdBy ?organisation ;
        pav:providedBy ?vendor .
    `,
    remove: {
      delete: `
        ?submission ?p ?o .
        ?submissionDocument ?sdp ?sdo .
        ?subject ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission ?p ?o .
        } UNION {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?subject ?fp ?fo .
        } UNION {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
    copy: {
      insert: `
        ?submission ?p ?o .
        ?submissionDocument ?sdp ?sdo .
        ?subject ?fp ?fo .
        ?artikel ?ap ?ao .
      `,
      where: `
        {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission ?p ?o .
        } UNION {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument ?sdp ?sdo .
        } UNION {
          ?subject ?fp ?fo .
        } UNION {
          ?submission <http://www.w3.org/ns/prov#generated> ?subject .
          ?submission <http://purl.org/dc/terms/subject> ?submissionDocument .
          ?submissionDocument <http://data.europa.eu/eli/ontology#has_part> ?artikel .
          ?artikel ?ap ?ao .
        }
      `,
    },
  },

  /////////////////////////////////////////////////////////////////////////////
  // Berichten
  // via berichten-melding-service
  /////////////////////////////////////////////////////////////////////////////

  /*
   * Failed jobs
   */

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

  /*
   * Success jobs
   */

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
          <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#url>
            ?bijlageDownloadLink .
      `,
      //NOTE: the UNION/FILTER are deliberate query optimisations, with complicated explanation.
      // So please be careful with them.
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

          BIND (CONCAT("${HOSTNAME}files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

           {
             ?subject ?p ?o .
           } UNION {
             ?conversatie ?pc ?co .
           } UNION {
             ?bericht ?pa ?ca .
           } UNION {
             ?bijlage ?pb ?ob .
           } UNION {
             ?physicalBijlage ?pp ?op .
           }
        }
        FILTER( REGEX(STR(?g), "LoketLB-berichtenGebruiker"))
      `,
    },
  },

  /*
   * For Berichten en Conversaties synced from Kaliope
   */

  {
    type: 'http://schema.org/Message',
    trigger: `
      ?subject
        <http://www.w3.org/ns/adms#status>
          <http://data.lblod.info/id/status/berichtencentrum/sync-with-kalliope/delivered/confirmed> .
    `,
    path: `
      ?subject <http://schema.org/recipient> ?organisation.
      ?organisation a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid>.

      ?vendor <http://mu.semte.ch/vocabularies/account/canActOnBehalfOf> ?organisation;
        a <http://xmlns.com/foaf/0.1/Agent>.
    `,
    //TODO: not sure when these conditions would be triggered;
    // Better to allow void delete statements
    // Note: if something weird would happen, it's gonna be a mess anyway and manual
    //   intervention will be needed.
    remove: {
      delete: `
          ?conversatie <http://schema.org/hasPart> ?subject.
      `,
      where: `
        GRAPH ?g {
          ?conversatie
            <http://schema.org/hasPart> ?subject ;
            a <http://schema.org/Conversation> .
        }
      `,
    },
    copy: {
      insert: `
        ?subject ?p ?o .
        ?conversatie ?pc ?co .
        ?bijlage ?pb ?ob .
        ?physicalBijlage ?pp ?op .

        ?bijlage
          <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#url>
            ?bijlageDownloadLink .
      `,
      //NOTE: the UNION/FILTER are deliberate query optimisations, with complicated explanation.
      // So please be careful with them.
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

          BIND (CONCAT("${HOSTNAME}files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

           {
             ?subject ?p ?o .
           } UNION {
             ?conversatie ?pc ?co .
           } UNION {
             ?bijlage ?pb ?ob .
           } UNION {
             ?physicalBijlage ?pp ?op .
           }
        }
        FILTER( REGEX(STR(?g), "LoketLB-berichtenGebruiker"))
      `,
    }
  },

  /*
   * For Berichten saved in Loket
   */

  {
    type: 'http://schema.org/Message',
    trigger: `
      ?subject
        <http://mu.semte.ch/vocabularies/ext/creator>
          <https://github.com/lblod/frontend-loket> .
    `,
    path: `
      ?subject <http://schema.org/sender> ?organisation.
      ?organisation a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid>.

      ?vendor <http://mu.semte.ch/vocabularies/account/canActOnBehalfOf> ?organisation;
        a <http://xmlns.com/foaf/0.1/Agent>.
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
          <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#url>
            ?bijlageDownloadLink .
      `,
      //NOTE: the UNION/FILTER are deliberate query optimisations, with complicated explanation.
      // So please be careful with them.
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

          BIND (CONCAT("${HOSTNAME}files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

           {
             ?subject ?p ?o .
           } UNION {
             ?conversatie ?pc ?co .
           } UNION {
             ?bijlage ?pb ?ob .
           } UNION {
             ?physicalBijlage ?pp ?op .
           }
        }
        FILTER( REGEX(STR(?g), "LoketLB-berichtenGebruiker"))
      `,
    }
  },

  /*
   * For Berichten saved in Loket, but via Controle as if you were ABB
   */

  {
    type: 'http://schema.org/Message',
    trigger: `
      ?subject
        <http://mu.semte.ch/vocabularies/ext/creator>
          <https://github.com/lblod/frontend-loket> ;
        <http://schema.org/sender>
          <http://data.lblod.info/id/bestuurseenheden/141d9d6b-54af-4d17-b313-8d1c30bc3f5b> .
    `,
    path: `
      ?subject <http://schema.org/recipient> ?organisation.
      ?organisation a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid>.

      ?vendor <http://mu.semte.ch/vocabularies/account/canActOnBehalfOf> ?organisation;
        a <http://xmlns.com/foaf/0.1/Agent>.
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
          <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#url>
            ?bijlageDownloadLink .
      `,
      //NOTE: the UNION/FILTER are deliberate query optimisations, with complicated explanation.
      // So please be careful with them.
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

          BIND (CONCAT("${HOSTNAME}files/", STR(?bijlageUUID), "/download") AS ?bijlageDownloadLink)

           {
             ?subject ?p ?o .
           } UNION {
             ?conversatie ?pc ?co .
           } UNION {
             ?bijlage ?pb ?ob .
           } UNION {
             ?physicalBijlage ?pp ?op .
           }
        }
        FILTER( REGEX(STR(?g), "LoketLB-berichtenGebruiker"))
      `,
    }
  },
];
