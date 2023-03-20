const {
  transformStatements,
  deleteFromIngetsGraph,
  insertIntoIngestGraph,
  deleteFromTargetGraph,
  insertIntoTargetGraph,
  insertIntoDebugGraph
} = require('./util');
const { BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES,
  DIRECT_DATABASE_ENDPOINT,
  MU_CALL_SCOPE_ID_INITIAL_SYNC,
  BATCH_SIZE,
  MAX_DB_RETRY_ATTEMPTS,
  SLEEP_BETWEEN_BATCHES,
  SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
  INGEST_GRAPH,
  FILE_SYNC_GRAPH
} = require('./config');
const { contextConfig } = require('./delta-context-config');


/**
* Dispatch the fetched information to a target graph.
* Note: <share://file/data> will be ADDED to it's own graph.
*   We take only care of adding them, not updating triples, this is a TODO
*  Deletes get types from ingest graph if not in delta
*  Inserts get types fron ingest graph if not in delta
*
*  Process deletes
*  - get changes with and without additional context
*  - execute mapped delete on target graph
*  - execute plain (without addtitional context)delete on ingest graph
*  - write (mapped) delete statements as inserts in debug-graph
*
*  Process inserts
*  - get context from ingest graph
*  - execute mapped insert into target graph
*  - execute plain insert into ingest
*
* @param { mu, muAuthSudo } lib - The provided libraries from the host service.
* @param { termObjectChangeSets: { deletes, inserts } } data - The fetched changes sets, which objects of serialized Terms
*          [ {
*              graph: "<http://foo>",
*              subject: "<http://bar>",
*              predicate: "<http://baz>",
*              object: "<http://boom>^^<http://datatype>"
*            }
*         ]
* @return {void} Nothing
*/

async function dispatch(lib, data) {
  const { mu, muAuthSudo, fetch } = lib;
  let { termObjectChangeSets, termObjectChangeSetsWithContext } = data;

  // Both arrays are the same length, so we can zip them together for easier processing
  zippedChangeSets = termObjectChangeSets.map((o, i) => ({
    original: o,
    withContext: termObjectChangeSetsWithContext[i]
  }));

  for (let { original, withContext } of zippedChangeSets) {
    const originalInserts = original.inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    const originalDeletes = original.deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);

    // Extra context needed for mapping from OP to DL model and filtering based on type.
    const deletesWithContext = withContext.inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    const insertsWithContext = withContext.deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);

    if (originalDeletes.length > 0) {
      // Map deletes from OP to DL model
      const transformedDeletes = await transformStatements(fetch, deletesWithContext);

      await deleteFromTargetGraph(lib, transformedDeletes);
      await insertIntoDebugGraph(lib, transformedDeletes);
      await deleteFromIngetsGraph(lib, originalDeletes);
    }

    if (originalInserts.length > 0) {
      // Map inserts from OP to DL model
      const transformedInserts = await transformStatements(fetch, insertsWithContext);

      await insertIntoTargetGraph(lib, transformedInserts);
      await insertIntoIngestGraph(lib, originalInserts);
    }
  }
}

module.exports = {
  dispatch
};