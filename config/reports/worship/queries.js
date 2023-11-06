import * as rst from 'rdf-string-ttl';

/**
 * Produces a query to get all the data for the given collection of subjects.
 *
 * @public
 * @function
 * @param {Array(String)} subjectURIs - An array of strings representing the
 * subjects you want all the data of. This means all the `?s ?p ?o` where `?s`
 * is one of the subjects in the array.
 * @returns {String} The query to execute.
 */
export function dataForSubjects(subjects) {
  const subjectsSparql = subjects.map(rst.termToString).join(' ');
  return `
SELECT DISTINCT ?s ?p ?o WHERE {
  VALUES ?s {
    ${subjectsSparql}
  }
  GRAPH ?g {
    ?s ?p ?o .
  }
}
  `;
}

/**
 * Produces a query that returns all RolBedienaren that where harvested. The
 * provenance is checked by the existence of a triple `?bedienaar
 * <http://www.w3.org/ns/prov#wasGeneratedBy>
 * <http://lblod.data.gift/id/app/lblod-harvesting> .`. Triples are also
 * filtered so that the ones from the temporary insert and delete graphs are
 * ignored.
 *
 * @public
 * @function
 * @returns {String} The SPARQL query to execute.
 */
export function allBedienaren() {
  return `
SELECT DISTINCT ?bedienaar WHERE {
  FILTER (?g NOT IN (
    <http://eredienst-mandatarissen-consumer/temp-inserts>,
    <http://eredienst-mandatarissen-consumer/temp-deletes>))
  GRAPH ?g {
    ?bedienaar
      a <http://data.lblod.info/vocabularies/erediensten/RolBedienaar> .
  }
}
  `;
}

/**
 * Produces a query that returns all EredienstMandatarissen that where
 * harvested. The provenance is checked by the existence of a triple
 * `?mandataris <http://www.w3.org/ns/prov#wasGeneratedBy>
 * <http://lblod.data.gift/id/app/lblod-harvesting> .`. Triples are also
 * filtered so that the ones from the temporary insert and delete graphs are
 * ignored.
 *
 * @public
 * @function
 * @returns {String} The SPARQL query to execute.
 */
export function allMandatarissen() {
  return `
SELECT DISTINCT ?mandataris WHERE {
  FILTER (?g NOT IN (
    <http://eredienst-mandatarissen-consumer/temp-inserts>,
    <http://eredienst-mandatarissen-consumer/temp-deletes>))
  GRAPH ?g {
    ?mandataris
      a <http://data.lblod.info/vocabularies/erediensten/EredienstMandataris> .
  }
}
  `;
}

/**
 * Produces a query that collects subjects related to a collection of given
 * subjects via a given predicate. The collected subjects will also be of the
 * given type. E.g. you have a list of books and you want to collect all the
 * authors: supply that list, a predicate such as `ont:writtenBy`, and
 * `ont:Author` as arguments to this function to get all the subjects of the
 * Author type that have written this books. There is no relation between the
 * books and the authors, just a new unordered collection of authors.
 *
 * @public
 * @function
 * @param {Array(String)} domainURIs - This collection contains subjects where you want related subjects of.
 * @param {String} predicate - Represents the full RDF predicate to get related objects that are new RDF entities.
 * @param {String} type - Represents the `rdf:type` of the related subject.
 * @returns {String} The SPARQL query to execute.
 */
export function domainToRange(domains, predicate, type) {
  const domainValues = domains.map(rst.termToString).join(' ');
  return `
SELECT DISTINCT ?domain ?range {
  VALUES ?domain {
    ${domainValues}
  }
  ?domain ${rst.termToString(predicate)} ?range .
  ?range a ${rst.termToString(type)} .
}
  `;
}

/*
 * Same as `domainToRange`, but backwards. This query now looks for entities
 * of a certain type that point to the URIs via a given predicate.
 * @see domainToRange
 * @public
 * @function
 * @param {Array(String)} domainURIs - This collection contains subjects where you want related subjects of.
 * @param {String} predicate - Represents the full RDF predicate to get related objects that are new RDF entities.
 * @param {String} type - Represents the `rdf:type` of the related subject.
 * @returns {String} The SPARQL query to execute.
 */
export function rangeToDomain(domains, predicate, type) {
  const domainValues = domains.map(rst.termToString).join(' ');
  return `
SELECT DISTINCT ?domain ?range {
  VALUES ?domain {
    ${domainValues}
  }
  ?range ${rst.termToString(predicate)} ?domain .
  ?range a ${rst.termToString(type)} .
}
  `;
}

/**
 * Produces a query to get all the triples from a graph. No pagination, just
 * everything at once.
 *
 * @public
 * @function
 * @param {String} graphURI - Represents the URI of the graph you want the
 * contents of.
 * @returns {String} The SPARQL query to execute.
 */
export function allFromGraph(graph) {
  return `
SELECT DISTINCT ?s ?p ?o WHERE {
  GRAPH ${rst.termToString(graph)} {
    ?s ?p ?o .
  }
}
  `;
}
