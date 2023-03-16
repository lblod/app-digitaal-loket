const { transformStatements, batchedDbUpdate, partition, deleteFromAllGraphs } = require('./util');
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


  console.log(`data:\n\n${JSON.stringify(data)}`)
  console.log(`termObjectChangeSets:\n\n${JSON.stringify(termObjectChangeSets)}`)


  console.log(`termObjectChangeSetsWithContext:\n\n${JSON.stringify(termObjectChangeSetsWithContext)}`)
  for (let { deletes, inserts } of termObjectChangeSets) {
    const originalInserts = inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    const originalDeletes = deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);

    console.log(`originalInserts:\n\n${JSON.stringify(originalInserts)}`)
    console.log(`originalDeletes:\n\n${JSON.stringify(originalDeletes)}`)
  }

  for (let { deletes, inserts } of termObjectChangeSetsWithContext) {
    const extendinserts = inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    const extenddeletes = deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);

    console.log(`extendinserts:\n\n${JSON.stringify(extendinserts)}`)
    console.log(`extenddeletes:\n\n${JSON.stringify(extenddeletes)}`)
  }

}

module.exports = {
  dispatch
};