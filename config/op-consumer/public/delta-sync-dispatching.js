const {
  transformStatements,
  deleteFromIngetsGraph,
  insertIntoIngestGraph,
  deleteFromTargetGraph,
  insertIntoTargetGraph,
  insertIntoDebugGraph
} = require('./util');

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
* @param { mu, muAuthSudo, fetch } lib - The provided libraries from the host service.
* @param { termObjectChangeSets: { deletes, inserts }, termObjectChangeSetsWithContext: { deletes, inserts } } data - The fetched changes sets, which objects of serialized Terms
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

  console.log(`Received ${zippedChangeSets.length} change sets`)
  // console.log(`Change sets:  ${JSON.stringify(zippedChangeSets)}`)

  for (let { original, withContext } of zippedChangeSets) {
    let originalDeletes, originalInserts, insertsWithContext, deletesWithContext;
    try {
      originalInserts = original.inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
      originalDeletes = original.deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    } catch (e) {
      console.log(`Error while processing change set: ${e}`);
      console.log(`Original change set: ${JSON.stringify(original)}`);
      throw e;
    }

    // Extra context needed for mapping from OP to DL model and filtering based on type.
    try {
      insertsWithContext = withContext.inserts.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
      deletesWithContext = withContext.deletes.map(o => `${o.subject} ${o.predicate} ${o.object}.`);
    } catch (e) {
      console.log(`Error while processing change set with context: ${e}`);
      console.log(`Change set with context: ${JSON.stringify(withContext)}`);
      throw e;
    }

    // console.log(`Original inserts: ${originalInserts}`)
    // console.log(`Original deletes: ${originalDeletes}`)
    // console.log(`Inserts with context: ${insertsWithContext}`)
    // console.log(`Deletes with context: ${deletesWithContext}`)

    if (originalDeletes.length) {
      // Map deletes from OP to DL model

      // console.log(`Transforming ${deletesWithContext.length} deletes`)
      // console.log(`Deletes with context:  ${JSON.stringify(deletesWithContext)}`)

      const transformedDeletes = await transformStatements(fetch, deletesWithContext);

      if (!transformedDeletes.length) {
        console.log(`Warn: Delete statements mapped to empty result.`);
        console.log(`Input: ${deletesWithContext}`);
        console.log(`Output: ${transformedDeletes}`);
      } else {
        await deleteFromTargetGraph(lib, transformedDeletes);
        await insertIntoDebugGraph(lib, transformedDeletes);
      }
      // await deleteFromIngetsGraph(lib, originalDeletes);
    }

    if (originalInserts.length) {
      // Map inserts from OP to DL model
      const transformedInserts = await transformStatements(fetch, insertsWithContext);

      if (!transformedInserts.length) {
        console.log(`Warn: Insert statements mapped to empty result.`);
        console.log(`Input: ${insertsWithContext}`);
        console.log(`Output: ${transformedInserts}`);
      } else {
        await insertIntoTargetGraph(lib, transformedInserts);
      }
      // await insertIntoIngestGraph(lib, originalInserts);
    }
  }
}

module.exports = {
  dispatch
};