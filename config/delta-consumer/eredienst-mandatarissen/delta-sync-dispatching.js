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
const { batchedDbUpdate } = require('./utils');
const endpoint = BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES
  ? DIRECT_DATABASE_ENDPOINT
  : process.env.MU_SPARQL_ENDPOINT; //Defaults to mu-auth
const map = require('./mappings.js');

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

/*
 * Maps the predicate or object of a triple onto a different value. This can be
 * used to make corrections (or deliberate mistakes to adhere to a different
 * version of a model ;) ) to triples before inserting them into a temporary
 * graph.
 *
 * @function
 * @param {Object} triple - A triple as given by the consumer. @see dispatch
 * @returns {Object} Same triple structure, but the predicate or object might
 * have changed.
 */
function mapTriple(triple) {
  //See if the class definition needs to be transformed
  if (
    triple.predicate === '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>' ||
    triple.predicate === 'a'
  ) {
    //Transform the class (object)
    const replacement = map.mappingsClasses.find(
      (elem) => elem.original === triple.object
    );
    if (replacement) triple.object = replacement.loket;
  } else {
    //Transform the predicate
    const replacement = map.mappingsPredicates.find(
      (elem) => elem.original === triple.predicate
    );
    if (replacement) triple.predicate = replacement.loket;
  }
  return triple;
}

module.exports = {
  dispatch,
};
