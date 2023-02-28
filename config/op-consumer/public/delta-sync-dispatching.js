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
  const { termObjectChangeSets } = data;

  for (let { deletes, inserts } of termObjectChangeSets) {
    const deleteStatements = deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    const insertStatements = inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);

    if (deleteStatements.length) {
      await transformStatements(fetch, deleteStatements).then(
        transformedStatements => {
          deleteFromAllGraphs(
            muAuthSudo.updateSudo,
            transformedStatements,
            { 'mu-call-scope-id': 'http://redpencil.data.gift/id/concept/muScope/deltas/write-for-dispatch' },
            process.env.MU_SPARQL_ENDPOINT, //Note: this is the default endpoint through auth
            MAX_DB_RETRY_ATTEMPTS,
            SLEEP_BETWEEN_BATCHES,
            SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
          );
        }
      )
    }


    if (insertStatements.length) {
      await transformStatements(fetch, insertStatements).then(
        transformedStatements => {
          batchedDbUpdate(
            muAuthSudo.updateSudo,
            INGEST_GRAPH,
            transformedStatements,
            { 'mu-call-scope-id': 'http://redpencil.data.gift/id/concept/muScope/deltas/write-for-dispatch' },
            process.env.MU_SPARQL_ENDPOINT, //Note: this is the default endpoint through auth
            BATCH_SIZE,
            MAX_DB_RETRY_ATTEMPTS,
            SLEEP_BETWEEN_BATCHES,
            SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
            "INSERT"
          );
        }
      )
    }
  }
}

module.exports = {
  dispatch
};