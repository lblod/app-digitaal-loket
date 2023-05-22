# Goal

Use public Organisatieportaal (OP) data in Loket.

This includes:

- Bestuurseenheden
- Bestuursorganen
- Mandaten en functies (but not the persons holding these mandates)

The first iteration focusses on worship data. This will gradually be extended.

# Consumer logic:

## Terminology

### Graphs

<dl>
  <dt>Ingest graph</dt>
  <dd>The graph in the loket application which mirrors OP producer data without any interpretation/mapping. Similar to a landingzone in ETL.</dd>
  <dt>Target graph</dt>
  <dd>The graph to which the mapped data is eventually written to</dd>
  <dt>Delete graph</dt>
  <dd>For debug purposes, triples deleted from the target graph are also inserted into a delete graph.</dd>
</dl>

### Deltas

<dl>
  <dt>Plain deltas aka termObjectChangeSets</dt>
  <dd>Inserts and delets as they are provided by the delta message</dd>
  <dt>Deltas with context aka termObjectChangeSetsWithContext</dt>
  <dd>Plain deltas which are extended with context. e.g. added rdf:types of the subject, or additional trippels needed for further processing. Which context - if any - gets added is defined by the delta-context-config.js</dd>
  <dt>Mapped deltas</dt>
  <dd>Delta's mapped to the dl model. This is done by sending the deltas with context to the reasoning service</dd>
</dl>

## Syncs

### Initial sync

1. Write the triples to the ingest graph - withouth any mapping or filtering
2. One-time reasoning run with the full ingest graph and write the results to the target graph (on-finish)

### Delta sync

For the deletes and inserts inside a termObjectChangeSet

Process deletes

1.  Get plain deletes and deletes with context from calling consumer app
2.  Send deletes with context to reasoner to be mapped.
3.  Delete plain deltas from ingest graph.
4.  Delete mapped - from step 2 - from the target graph.
5.  Insert the same mapped delete statements to the Delete graph for debug purposes.

Process inserts

1.  Get plain inserts and inserts with context from calling consumer app
2.  Send inserts with context to reasoner to be mapped.
3.  Write plain inserts into the ingest graph
4.  Write the mapped inserts into the target graph
