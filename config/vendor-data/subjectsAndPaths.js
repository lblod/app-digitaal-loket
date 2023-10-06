export const subjects = [
  {
    type: 'http://rdf.myexperiment.org/ontologies/base/Submission',
    path: `
      ?subject
        pav:createdBy ?organisation ;
        pav:providedBy ?vendor .
    `,
  },
  {
    type: 'http://vocab.deri.ie/cogs#Job',
    path: `
      ?subject
        <http://purl.org/pav/providedBy> ?vendor ;
        <http://schema.org/sender> ?organisation .
    `,
  },
  {
    type: 'http://schema.org/Message',
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
    // Better when counting on the existence of Jobs and Tasks for the Message:
    //path: `
    //  ?ic
    //    <http://purl.org/dc/terms/subject> ?subject ;
    //    a <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#DataContainer> ;
    //    <http://purl.org/pav/providedBy> ?vendor ;
    //    <http://schema.org/sender> ?organisation .
    //`,
  },
  {
    type: 'http://schema.org/Conversation',
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
  },
];
