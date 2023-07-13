const map = require('./mappings.js');

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

async function batchedDbUpdate(
  muUpdate,
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
      await muUpdate(
        `
        ${operation} DATA {
          GRAPH <${graph}> {
            ${batch}
          }
        }
      `,
        extraHeaders,
        endpoint
      );
    };

    await dbOperationWithRetry(insertCall, 0, maxAttempts, sleepTimeOnFail);

    console.log(`Sleeping before next query execution: ${sleepBetweenBatches}`);
    await new Promise((r) => setTimeout(r, sleepBetweenBatches));
  }
}

async function dbOperationWithRetry(
  callback,
  attempt,
  maxAttempts,
  sleepTimeOnFail
) {
  try {
    return await callback();
  } catch (e) {
    console.log(
      `Operation failed for ${callback.toString()}, attempt: ${attempt} of ${maxAttempts}`
    );
    console.log(`Error: ${e}`);
    console.log(`Sleeping ${sleepTimeOnFail} ms`);

    if (attempt >= maxAttempts) {
      console.log(`Max attempts reached for ${callback.toString()}, giving up`);
      throw e;
    }

    await new Promise((r) => setTimeout(r, sleepTimeOnFail));
    return dbOperationWithRetry(
      callback,
      ++attempt,
      maxAttempts,
      sleepTimeOnFail
    );
  }
}

module.exports = {
  batchedDbUpdate,
  mapTriple,
};
