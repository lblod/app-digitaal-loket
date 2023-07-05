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
export function dataForSubjects(subjectURIs) {
  const subjectValues = subjectURIs.map((uri) => `<${uri}>`).join(' ');
  return `
SELECT ?g ?s ?p ?o WHERE {
  VALUES ?s {
    ${subjectValues}
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
  ?bedienaar
    a <http://data.lblod.info/vocabularies/erediensten/RolBedienaar> ;
    <http://www.w3.org/ns/prov#wasGeneratedBy>
      <http://lblod.data.gift/id/app/lblod-harvesting> .
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
  ?mandataris
    a <http://data.lblod.info/vocabularies/erediensten/EredienstMandataris> ;
    <http://www.w3.org/ns/prov#wasGeneratedBy>
      <http://lblod.data.gift/id/app/lblod-harvesting> .
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
export function subjectToRange(domainURIs, predicate, type) {
  const subjectValues = domainURIs.map((uri) => `<${uri}>`).join(' ');
  return `
SELECT DISTINCT ?range {
  VALUES ?s {
    ${subjectValues}
  }
  ?s <${predicate}> ?range .
  ?range a <${type}> .
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
export function allFromGraph(graphURI) {
  return `
SELECT DISTINCT ?s ?p ?o WHERE {
  GRAPH <${graphURI}> {
    ?s ?p ?o .
  }
}
  `;
}

//Reasonable performance, but data is missing. For example, if a geboorte is
//missing, nothing is inluded in the results. Using optional on these
//statements tanks performance to a point where it's unusable.
//function getBedienarenSubjects(limit, offset) {
//  return `
//SELECT DISTINCT ?bedienaar ?persoon ?contact ?identifier ?geboorte ?adres WHERE {
//  {
//    SELECT DISTINCT ?bedienaar WHERE {
//      FILTER (?g NOT IN (
//        <http://eredienst-mandatarissen-consumer/temp-inserts>,
//        <http://eredienst-mandatarissen-consumer/temp-deletes>))
//      ?bedienaar a <http://data.lblod.info/vocabularies/erediensten/RolBedienaar> .
//    }
//    LIMIT ${limit}
//    OFFSET ${offset}
//  }
//  ?bedienaar <http://www.w3.org/ns/org#heldBy> ?persoon ;
//             <http://schema.org/contactPoint> ?contact .
//  ?persoon <http://www.w3.org/ns/adms#identifier> ?identifier ;
//           <http://data.vlaanderen.be/ns/persoon#heeftGeboorte> ?geboorte .
//  ?contact <http://www.w3.org/ns/locn#address> ?adres .
//}
//  `;
//}

//This query gets all information at once, but too heavy for Virtuoso and hard
//to maintain.
//function getquery(limit, offset) {
//  return `
//PREFIX schema:     <http://schema.org/>
//PREFIX rdf:        <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
//PREFIX org:        <http://www.w3.org/ns/org#>
//PREFIX contactHub: <http://data.lblod.info/vocabularies/contacthub/>
//PREFIX person:     <http://www.w3.org/ns/person#>
//PREFIX persoon:    <http://data.vlaanderen.be/ns/persoon#>
//PREFIX adms:       <http://www.w3.org/ns/adms#>
//PREFIX foaf:       <http://xmlns.com/foaf/0.1/>
//PREFIX locn:       <http://www.w3.org/ns/locn#>
//PREFIX adres:      <https://data.vlaanderen.be/ns/adres#>
//PREFIX skos:       <http://www.w3.org/2004/02/skos/core#>
//
//SELECT * WHERE {
//  {
//    SELECT DISTINCT ?bedienaar WHERE {
//      FILTER (?g NOT IN (
//        <http://eredienst-mandatarissen-consumer/temp-inserts>,
//        <http://eredienst-mandatarissen-consumer/temp-deletes>))
//      ?bedienaar a <http://data.lblod.info/vocabularies/erediensten/RolBedienaar> .
//    }
//    LIMIT ${limit}
//    OFFSET ${offset}
//  }
//  GRAPH ?g {
//  	?bedienaar
//      rdf:type ?bedienaarType ;
//      org:holds ?mandataris ;
//      org:heldBy ?persoon ;
//      schema:contactPoint ?contact .
//    OPTIONAL { ?bedienaar contactHub:startdatum ?startDate . }
//    OPTIONAL { ?bedienaar contactHub:eindedatum ?endDate . }
//
//    ?persoon
//      rdf:type person:Person ;
//      rdf:type ?persoonType ;
//      adms:identifier ?identifier .
//    OPTIONAL { ?persoon foaf:familyName ?familyName . }
//    OPTIONAL { ?persoon persoon:gebruikteVoornaam ?firstName . }
//    OPTIONAL { ?persoon persoon:heeftNationaliteit ?nationaliteit . }
//    OPTIONAL { ?persoon persoon:geslacht ?geslacht . }
//    OPTIONAL { ?persoon persoon:heeftGeboorte ?geboorte . }
//    ?contact
//      rdf:type schema:ContactPoint ;
//      rdf:type ?contactType ;
//      locn:address ?adres .
//    OPTIONAL { ?contact schema:contactType ?contactSort . }
//    OPTIONAL { ?contact schema:email ?email . }
//    OPTIONAL { ?contact schema:telephone ?phone . }
//
//    ?adres
//      rdf:type locn:Address ;
//      rdf:type ?adresType .
//    OPTIONAL { ?adres adres:Adresvoorstelling.busnummer ?busnummer . }
//    OPTIONAL { ?adres adres:Adresvoorstelling.huisnummer ?huisnummer . }
//    OPTIONAL { ?adres locn:thoroughfare ?straat . }
//    OPTIONAL { ?adres locn:postCode ?postcode . }
//    OPTIONAL { ?adres adres:gemeentenaam ?stad . }
//    OPTIONAL { ?adres adres:land ?land . }
//    OPTIONAL { ?adres locn:fullAddress ?volAdress . }
//    OPTIONAL { ?adres adres:verwijstNaar ?adresVerwijzing . }
//
//    ?geboorte
//      rdf:type persoon:Geboorte .
//    OPTIONAL { ?geboorte persoon:datum ?geboorteDatum . }
//
//    ?identifier
//      rdf:type adms:Identifier .
//    OPTIONAL { ?identifier skos:notation ?rrnummer . }
//  }
//}
//  `;
//}
