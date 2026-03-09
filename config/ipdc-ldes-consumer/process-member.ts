import type { Client } from "ldes-client";

import { DataFactory, type Quad_Subject } from "n3";
const { namedNode, quad, variable } = DataFactory;

import type * as RDF from "@rdfjs/types";

import {
  MU_APPLICATION_GRAPH,
  SPARQL_BATCH_SIZE,
  ENABLE_SPARQL_BATCHING,
  // @ts-expect-error from service
} from "../cfg";
//@ts-expect-error from service
import { executeDeleteQuery } from "../lib/sparql-queries";
//@ts-expect-error from service
import { convertBlankNodes } from "../lib/utils";
//@ts-expect-error from service
import { sparqlEscapeString, sparqlEscapeUri } from "mu";
// @ts-expect-error from service
import { updateSudo } from "@lblod/mu-auth-sudo";

// ldes-client doesn't expose the `Member` type directly...
type Member =
  ReturnType<Client["stream"]> extends ReadableStream<infer M> ? M : never;

/**
 * Custom `processMember` function. Expects `INGEST_MODE` to be `MATERIALIZE`
 */
export async function processMember(member: Member) {
  member.quads = convertBlankNodes(member.quads);
  const quadsToAdd: RDF.Quad[] = member.quads;
  const quadsToRemove: RDF.Quad[] = [
    quad(member.id as Quad_Subject, variable("p"), variable("o")),
  ];
  await executeDeleteQuery(quadsToRemove);
  await executeInsertQuery(quadsToAdd);
}


/**
 * Custom toString function which treats `mu:uuid` attribute slightly differently to get around a virtuoso bug
 */
export function toString(term: RDF.Term): string {
  switch (term.termType) {
    case "NamedNode":
      return sparqlEscapeUri(term.value);
    case "Literal": {
      let result = sparqlEscapeString(term.value);

      if (term.language) result += `@${term.language}`;
      else if (term.datatype) {
        result += `^^${sparqlEscapeUri(term.datatype.value)}`;
      }
      return result;
    }
    case "Quad":
      // Hack/workaround as virtuoso and mu-cl-resources expect mu:uuid attributes to have no datatype
      // Virtuoso doesn't treat untyped literals and literals with the datatype `xsd:string` as equivalent
      // See https://github.com/openlink/virtuoso-opensource/issues/728
      if (
        term.predicate.equals(
          namedNode("http://mu.semte.ch/vocabularies/core/uuid"),
        ) &&
        term.object.termType === "Literal"
      ) {
        return `${toString(term.subject)} ${toString(term.predicate)} ${sparqlEscapeString(term.object.value)}.`;
      } else {
        return `${toString(term.subject)} ${toString(
          term.predicate,
        )} ${toString(term.object)}.`;
      }

    case "Variable":
      return `?${term.value}`;
    default:
      return term.value;
  }
}

function constructTriplesString(quads: RDF.Quad[]) {
  const triplesString = quads
    .map(toString)
    .filter((item, index, array) => array.indexOf(item) === index)
    .join("\n        ");
  return triplesString;
}

export function constructInsertQuery(quads: RDF.Quad[]) {
  const triplesString = constructTriplesString(quads);
  const sparqlQuery = `INSERT DATA {
    GRAPH <${MU_APPLICATION_GRAPH}> {
        ${triplesString}
    }
}`;
  return sparqlQuery;
}

async function executeInsertQuery(quads: RDF.Quad[]) {
  let nBatches;
  let batchSize;
  if (ENABLE_SPARQL_BATCHING) {
    nBatches =
      Math.floor(quads.length / SPARQL_BATCH_SIZE) +
      (quads.length % SPARQL_BATCH_SIZE ? 1 : 0);
    batchSize = SPARQL_BATCH_SIZE;
  } else {
    nBatches = quads.length ? 1 : 0;
    batchSize = quads.length;
  }

  for (let index = 0; index < nBatches; index++) {
    const iQuads = index * batchSize;
    const quadsBatch = quads.slice(iQuads, iQuads + batchSize);
    const queryStr = constructInsertQuery(quadsBatch);
    await updateSudo(queryStr);
  }
}
