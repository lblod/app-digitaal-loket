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

    await dbOperationWithRetry(insertCall, 0, maxAttempts, sleepTimeOnFail);

    console.log(`Sleeping before next query execution: ${sleepBetweenBatches}`);
    await new Promise(r => setTimeout(r, sleepBetweenBatches));
  }
}

async function dbOperationWithRetry(callback,
                                    attempt,
                                    maxAttempts,
                                    sleepTimeOnFail){
  try {
    return await callback();
  }
  catch(e){
    console.log(`Operation failed for ${callback.toString()}, attempt: ${attempt} of ${maxAttempts}`);
    console.log(`Error: ${e}`);
    console.log(`Sleeping ${sleepTimeOnFail} ms`);

    if(attempt >= maxAttempts){
      console.log(`Max attempts reached for ${callback.toString()}, giving up`);
      throw e;
    }

    await new Promise(r => setTimeout(r, sleepTimeOnFail));
    return dbOperationWithRetry(callback, ++attempt, maxAttempts, sleepTimeOnFail);
  }
}

module.exports = {
  batchedDbUpdate
};
