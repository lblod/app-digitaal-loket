# Copying data about Messages to Vendor Graph

The query below shows, in one single query, which data to move to the vendor
graphs about Messages, Conversations, Attachments and Jobs. This query is
intensly heavy for Virtuoso and needed te be split up in more sizable chunks.
The way this can be done is to comment/remove statements in the WHERE-clause to
make the amount of triples smaller and execute a query for each of the
uncommented line. This is done by creating copies of the same query with
different statements uncommented. (Since the Migration service does not like
comments, the commented lines are removed.)

The first part of the query up until the OPTIONAL Job should always be the
same. This part forms a tree of subjects from which all the information needs
to be copied. The last part of the query then lists all the subjects and
queries their information. This part can be done in steps and separated in
multiple queries.

We can play around with the optional Job. No need to include them in every
query; we can also just copy the jobs as a separate query.

Look at the SPARQL queries in this folder and compare the differences in the
last part of the query.

```sparql
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sch:     <http://schema.org/>
PREFIX nie:     <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
PREFIX nfo:     <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX adms:    <http://www.w3.org/ns/adms#>
PREFIX deli:    <http://data.lblod.info/id/status/berichtencentrum/sync-with-kalliope/delivered/>
PREFIX js:      <http://redpencil.data.gift/id/concept/JobStatus/>
PREFIX task:    <http://redpencil.data.gift/vocabularies/tasks/>
PREFIX dct:     <http://purl.org/dc/terms/>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX account: <http://mu.semte.ch/vocabularies/account/>
PREFIX core:    <http://mu.semte.ch/vocabularies/core/>

INSERT {
  GRAPH ?vendorGraph {
    ?conversation ?pc ?oc .
    ?message ?pm ?om .
    ?attachment ?pa ?oa .
    ?physicalAttachment ?pp ?op .
    ?job ?pj ?oj .
  }
}
WHERE {
  GRAPH <http://mu.semte.ch/graphs/public> {
    ?organisation
      a besluit:Bestuurseenheid ;
      core:uuid ?organisationUUID .
  }
  GRAPH <http://mu.semte.ch/graphs/automatic-submission> {
    ?vendor
      account:canActOnBehalfOf ?organisation ;
      core:uuid ?vendorUUID .
  }
  BIND (IRI(CONCAT("http://mu.semte.ch/graphs/vendors/", STR(?vendorUUID), "/", STR(?organisationUUID))) AS ?vendorGraph)
  BIND (IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", STR(?organisationUUID), "/LoketLB-berichtenGebruiker")) AS ?organisationGraph)
  GRAPH ?organisationGraph {
    ?conversation
      a sch:Conversation ;
      sch:hasPart ?message .
    ?message
      a sch:Message ;
      nie:hasPart ?attachment .
    ?attachment
      a nfo:FileDataObject .
    ?physicalAttachment
      a nfo:FileDataObject ;
      nie:dataSource ?attachment .

    #Exclude unconfirmed or failed messages from Kaliope
    FILTER NOT EXISTS {
      ?message adms:status deli:unconfirmed .
    }
    FILTER NOT EXISTS {
      ?message adms:status deli:failedConfirmation .
    }

    #Include the optional Job for the Berichten-Melding
    OPTIONAL {
      ?job
        dct:subject ?message ;
        adms:status js:success ;
        task:operation js:harvestBericht ;
        ?pj ?oj .
    }

    ?conversation ?pc ?oc .
    ?message ?pm ?om .
    ?attachment ?pa ?oa .
    ?physicalAttachment ?pp ?op .
  }
}
```
