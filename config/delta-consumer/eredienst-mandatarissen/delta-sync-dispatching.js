const {
  BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES,
  DIRECT_DATABASE_ENDPOINT,
  MU_CALL_SCOPE_ID_INITIAL_SYNC,
  BATCH_SIZE,
  MAX_DB_RETRY_ATTEMPTS,
  SLEEP_BETWEEN_BATCHES,
  SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
  INGEST_GRAPH,
} = require('./config');
const { batchedDbUpdate, mapTriple } = require('./utils');
const endpoint = BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES
  ? DIRECT_DATABASE_ENDPOINT
  : process.env.MU_SPARQL_ENDPOINT; //Defaults to mu-auth

/**
 * Dispatch the fetched triples to a target graph.
 *
 * @public
 * @async
 * @function
 * @param { { mu, muAuthSudo } } lib - The provided libraries from the host
 * service.
 * @param { Array({ deletes, inserts }) } data - The fetched changesets, with
 * objects of serialized Quads. E.g.
 * [
 *   {
 *     graph: "<http://foo>",
 *     subject: "<http://bar>",
 *     predicate: "<http://baz>",
 *     object: "<http://boom>^^<http://datatype>"
 *   }, ...
 * ]
 * @return {undefined} Nothing
 */
async function dispatch(lib, data) {
  for (const { deletes, inserts } of data.termObjectChangeSets) {
    if (BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES) {
      console.warn('Service configured to skip MU_AUTH!');
    }
    console.info(`Using ${endpoint} to insert triples`);

    await processDeletes(lib, deletes);
    await processInserts(lib, inserts);
  }
}

async function processInserts(lib, data) {
  const { muAuthSudo } = lib;

  const insertStatements = data
    .map(mapTriple)
    .map((o) => `${o.subject} ${o.predicate} ${o.object}.`);
  await batchedDbUpdate(
    muAuthSudo.updateSudo,
    `${INGEST_GRAPH}-inserts`,
    insertStatements,
    {},
    endpoint,
    BATCH_SIZE,
    MAX_DB_RETRY_ATTEMPTS,
    SLEEP_BETWEEN_BATCHES,
    SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
    'INSERT'
  );
}

async function processDeletes(lib, data) {
  const { muAuthSudo } = lib;

  const deleteStatements = data
    .map(mapTriple)
    .map((o) => `${o.subject} ${o.predicate} ${o.object}.`);
  await batchedDbUpdate(
    muAuthSudo.updateSudo,
    `${INGEST_GRAPH}-deletes`,
    deleteStatements,
    {},
    endpoint,
    BATCH_SIZE,
    MAX_DB_RETRY_ATTEMPTS,
    SLEEP_BETWEEN_BATCHES,
    SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
    'INSERT' //Yes, INSERT
  );
}



module.exports = {
  dispatch,
};
