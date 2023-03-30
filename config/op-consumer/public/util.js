const {
  MAX_REASONING_RETRY_ATTEMPTS,
  SLEEP_TIME_AFTER_FAILED_REASONING_OPERATION,
  MAX_DB_RETRY_ATTEMPTS,
  SLEEP_BETWEEN_BATCHES,
  SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
  TARGET_GRAPH,
  DCR_LANDING_ZONE_GRAPH,
  DELETE_GRAPH,
  BATCH_SIZE,
  DCR_LANDING_ZONE_DATABASE_ENDPOINT,
  TARGET_DATABASE_ENDPOINT
} = require('./config');

async function batchedDbUpdate(muUpdate,
  graph,
  triples,
  extraHeaders,
  endpoint,
  batchSize,
  maxAttempts,
  sleepBetweenBatches = 1000,
  sleepTimeOnFail = 1000,
  operation = 'INSERT'
) {

  for (let i = 0; i < triples.length; i += batchSize) {
    console.log(`Inserting triples in batch: ${i}-${i + batchSize}`);

    const batch = triples.slice(i, i + batchSize).join('\n');

    const insertCall = async () => {
      await muUpdate(`
${operation} DATA {
GRAPH <${graph}> {
${batch}
}
}
`, extraHeaders, endpoint);
    };

    await operationWithRetry(insertCall, 0, maxAttempts, sleepTimeOnFail);

    console.log(`Sleeping before next query execution: ${sleepBetweenBatches}`);
    await new Promise(r => setTimeout(r, sleepBetweenBatches));
  }
}

async function operationWithRetry(callback,
  attempt,
  maxAttempts,
  sleepTimeOnFail) {
  try {
    if (typeof callback === "function")
      return await callback();
    else // Catch error from promise - not how I would do it normally, but allows re use of existing code.
      return await callback;
  }
  catch (e) {
    console.log(`Operation failed for ${callback.toString()}, attempt: ${attempt} of ${maxAttempts}`);
    console.log(`Error: ${e}`);
    console.log(`Sleeping ${sleepTimeOnFail} ms`);

    if (attempt >= maxAttempts) {
      console.log(`Max attempts reached for ${callback.toString()}, giving up`);
      throw e;
    }

    await new Promise(r => setTimeout(r, sleepTimeOnFail));
    return operationWithRetry(callback, ++attempt, maxAttempts, sleepTimeOnFail);
  }
}

/**
 * Splits an array into two parts, a part that passes and a part that fails a predicate function.
 * Credits: https://github.com/benjay10
 * @public
 * @function partition
 * @param {Array} arr - Array to be partitioned
 * @param {Function} fn - Function that accepts single argument: an element of the array, and should return a truthy or falsy value.
 * @returns {Object} Object that contains keys passes and fails, each representing an array with elemets that pass or fail the predicate respectively
 */
function partition(arr, fn) {
  let passes = [], fails = [];
  arr.forEach((item) => (fn(item) ? passes : fails).push(item));
  return { passes, fails };
}


/**
 * Send triples to reasoning service for conversion
 *
 */
function transformTriples(fetch, triples) {
  return operationWithRetry(mainConversion(fetch, triples), 0,
    MAX_REASONING_RETRY_ATTEMPTS, SLEEP_TIME_AFTER_FAILED_REASONING_OPERATION);
}


function mainConversion(fetch, triples) {
  let formdata = new URLSearchParams();
  formdata.append("data", triples);

  let requestOptions = {
    method: 'POST',
    body: formdata,
    redirect: 'follow'
  };

  return fetch("http://reasoner/reason/op2dl/main", requestOptions)
    .then(response => response.text());
}

function transformStatements(fetch, triples) {
  console.log(`Received ${JSON.stringify(triples)} to transform`);
  return transformTriples(fetch, triples.join('\n')).then(
    graph => {
      statements = graph ? graph.replace(/\n{2,}/g, '').split('\n') : [];
      console.log(`CONVERSION: FROM ${triples.length} triples to ${statements.length}`);
      return statements;
    }
  )
}

async function deleteFromTargetGraph(lib, statements) {
  console.log(`Deleting ${statements.length} statements from target graph`);
  console.log(`Statements:  ${JSON.stringify(statements)}`)
  await batchedDbUpdate(
    lib.muAuthSudo.updateSudo,
    TARGET_GRAPH,
    statements,
    {},
    TARGET_DATABASE_ENDPOINT,
    BATCH_SIZE,
    MAX_DB_RETRY_ATTEMPTS,
    SLEEP_BETWEEN_BATCHES,
    SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
    'DELETE');
}

async function insertIntoTargetGraph(lib, statements) {
  console.log(`Inserting ${statements.length} statements into target graph`);
  console.log(`Statements:  ${JSON.stringify(statements)}`)

  await batchedDbUpdate(
    lib.muAuthSudo.updateSudo,
    TARGET_GRAPH,
    statements,
    {},
    TARGET_DATABASE_ENDPOINT,
    BATCH_SIZE,
    MAX_DB_RETRY_ATTEMPTS,
    SLEEP_BETWEEN_BATCHES,
    SLEEP_TIME_AFTER_FAILED_DB_OPERATION,
    'INSERT');
}

async function transformLandingZoneGraph(fetch) {
  console.log(`Transforming landing zone graph: ${DCR_LANDING_ZONE_GRAPH}`);

  const response = await fetch(`http://reasoner/reason/op2dl/main?data=${encodeURIComponent(`${DCR_LANDING_ZONE_DATABASE_ENDPOINT}?default-graph-uri=&query=CONSTRUCT+%7B%0D%0A%3Fs+%3Fp+%3Fo%0D%0A%7D+WHERE+%7B%0D%0A+GRAPH+<${DCR_LANDING_ZONE_GRAPH}>+%7B%0D%0A%3Fs+%3Fp+%3Fo%0D%0A%7D%0D%0A%7D&should-sponge=&format=text%2Fplain&timeout=0&run=+Run+Query`)}`);
  const text = await response.text();
  const statements = text.replace(/\n{2,}/g, '').split('\n');

  return statements;
}

module.exports = {
  batchedDbUpdate,
  partition,
  transformStatements,
  transformLandingZoneGraph,
  deleteFromTargetGraph,
  insertIntoTargetGraph,
};