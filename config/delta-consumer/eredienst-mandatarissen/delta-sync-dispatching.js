const { BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES,
        DIRECT_DATABASE_ENDPOINT,
        MU_CALL_SCOPE_ID_INITIAL_SYNC,
        BATCH_SIZE,
        MAX_DB_RETRY_ATTEMPTS,
        SLEEP_BETWEEN_BATCHES,
        SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
        INGEST_GRAPH
      } = require('./config');
const { batchedDbUpdate } = require('./utils');
const endpoint = BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES ? DIRECT_DATABASE_ENDPOINT : process.env.MU_SPARQL_ENDPOINT; //Defaults to mu-auth


/**
 * Dispatch the fetched information to a target graph.
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
async function dispatch(lib, data){
  const { mu, muAuthSudo } = lib;
  const { termObjectChangeSets } =  data;

  for (let { deletes, inserts } of termObjectChangeSets) {

    if(BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES){
      console.warn(`Service configured to skip MU_AUTH!`);
    }
    console.log(`Using ${endpoint} to insert triples`);

    const deleteStatements = deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    await batchedDbUpdate(
      muAuthSudo.updateSudo,
      INGEST_GRAPH,
      deleteStatements,
      { },
      endpoint,
      BATCH_SIZE,
      MAX_DB_RETRY_ATTEMPTS,
      SLEEP_BETWEEN_BATCHES,
      SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
      "DELETE"
     );
    const insertStatements = inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    await batchedDbUpdate(
      muAuthSudo.updateSudo,
      INGEST_GRAPH,
      insertStatements,
      { },
      endpoint,
      BATCH_SIZE,
      MAX_DB_RETRY_ATTEMPTS,
      SLEEP_BETWEEN_BATCHES,
      SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
      "INSERT"
     );
  }
}

module.exports = {
  dispatch
};
