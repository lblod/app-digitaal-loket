const BATCH_SIZE = parseInt(process.env.BATCH_SIZE) || 100;
const MU_CALL_SCOPE_ID_INITIAL_SYNC = process.env.MU_CALL_SCOPE_ID_INITIAL_SYNC || 'http://redpencil.data.gift/id/concept/muScope/deltas/consumer/initialSync';
const BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES = process.env.BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES == 'true' ? true : false;
const DIRECT_DATABASE_ENDPOINT = process.env.DIRECT_DATABASE_ENDPOINT || 'http://virtuoso:8890/sparql';
const MAX_DB_RETRY_ATTEMPTS = parseInt(process.env.MAX_DB_RETRY_ATTEMPTS || 5);
const SLEEP_BETWEEN_BATCHES = parseInt(process.env.SLEEP_BETWEEN_BATCHES || 1000);
const SLEEP_TIME_AFTER_FAILED_DB_OPERATION = parseInt(process.env.SLEEP_TIME_AFTER_FAILED_DB_OPERATION || 60000);
const INGEST_GRAPH = process.env.INGEST_GRAPH || `http://mu.semte.ch/graphs/public`;

module.exports = {
  BATCH_SIZE,
  MU_CALL_SCOPE_ID_INITIAL_SYNC,
  BYPASS_MU_AUTH_FOR_EXPENSIVE_QUERIES,
  DIRECT_DATABASE_ENDPOINT,
  MAX_DB_RETRY_ATTEMPTS,
  SLEEP_BETWEEN_BATCHES,
  SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
  INGEST_GRAPH
};
