import { querySudo, updateSudo } from "@lblod/mu-auth-sudo";
const DROP_GRAPH_BATCH_SIZE = 50;
const sparqlOptions = {
  sparqlEndpoint: "http://virtuoso:8890/sparql",
  mayRetry: true,
};


async function dropLMBGraphs() {
  const count = await countUnimportantGraphs();
  console.log(`dropping ${count} LMB graphs`);

  let droppedGraphs = 1;
  let droppedSoFar = 0;
  while (droppedGraphs) {
    droppedGraphs = await dropBatchOfLMBGraphs();
    droppedSoFar += droppedGraphs;
    console.log(`Dropped ${droppedSoFar}/${count} graphs... (${((droppedSoFar / count) * 100).toFixed(2)}%)`);
  }
  console.log(`Dropped all LMB graphs!`);
}

const graphTargetter = `
  # using this instead of ?s ?p ?o for faster counting
  {?s org:holds ?mandaat.}
  UNION
  { ?s a <http://data.vlaanderen.be/ns/mandaat#Fractie>. }
  UNION
  { ?s a <http://www.w3.org/ns/person#Person>. }
  UNION
  { ?s a <http://www.w3.org/ns/adms#Identifier>.}
`;

async function countUnimportantGraphs() {
    const result = await querySudo(`
    PREFIX org: <http://www.w3.org/ns/org#>

    SELECT COUNT(DISTINCT(?g)) as ?count WHERE {
      GRAPH ?g {
        ${graphTargetter}
      }
      FILTER (STRENDS(STR(?g), "/LoketLB-mandaatGebruiker"))
    }`, {}, sparqlOptions);
    return parseInt(result.results.bindings[0].count.value);
}

async function getUnimportantGraphs() {
  const result = await querySudo(
    `
    PREFIX org: <http://www.w3.org/ns/org#>
    SELECT DISTINCT ?g WHERE {
      GRAPH ?g {
        ${graphTargetter}
      }
      FILTER (STRENDS(STR(?g), "/LoketLB-mandaatGebruiker"))
    } LIMIT ${DROP_GRAPH_BATCH_SIZE}
  `,
    {},
    sparqlOptions
  );

  return result.results.bindings.map((b) => b.g.value);
}

async function dropBatchOfLMBGraphs() {
  const graphs = await getUnimportantGraphs();
  if (graphs.length === 0) {
    return 0;
  }
  const dropStatement = `DROP SILENT GRAPH <${graphs.join(
    ">; DROP SILENT GRAPH <"
  )}>;`;
  await updateSudo(dropStatement, {}, sparqlOptions);
  return graphs.length;
}

async function transform() {
  console.log(
    "\n\nWARNING: this script will DELETE all LMB data. Ctrl-C to cancel in the next 10 seconds\n\n"
  );
  await new Promise((resolve) => setTimeout(resolve, 10000));

  console.log("transforming!");
  await dropLMBGraphs();
  console.log("done transforming!");
}

transform().catch((e) => console.log(e));
