module.exports = {
  name: 'climate-subsidy-extractor',
  execute: async (store, graphs, lib, form) => {
    const { $rdf, mu, sudo } = lib;
    const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const DBPEDIA = new $rdf.Namespace('http://dbpedia.org/ontology/');
    const { population, drawingRight } = await getBestuurInfo(form.uri, mu, sudo);

    if (population && drawingRight) {
      store.add($rdf.sym(form.uri), DBPEDIA('populationTotal'), population, graphs.additions);
      store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('drawingRight'), drawingRight, graphs.additions);
      store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('lekp'), '1.0', graphs.additions);
    }
  }
};

// TODO: now STUB values are given, because no information is provided
async function getBestuurInfo(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX dbpedia: <http://dbpedia.org/ontology/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX m8g: <http://data.europa.eu/m8g/>
    PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
    PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

    SELECT DISTINCT ?population ?drawingRight where {
      GRAPH ?h {
        ${mu.sparqlEscapeUri(formUri)} <http://purl.org/dc/terms/isPartOf> ?flowStep .
        ?subsidyAmount
          subsidie:maximumbedrag ?drawingRight ;
          dct:requires ?requirementGroup .
        ?requirementGroup
          m8g:hasCriterionRequirement ?criterionRequirementBestuurseenheid ;
          m8g:hasCriterionRequirement ?criterionRequirementPopulation .
        ?criterionRequirementPopulation a m8g:CriterionRequirement ;
          dbpedia:populationTotal ?population .
      }
      GRAPH ?g {
        ?flowStep <http://purl.org/dc/terms/references> ?proceduralStep .
        ?criterion <http://purl.org/dc/terms/isPartOf> ?proceduralStep ;
          m8g:fulfilledByRequirementGroup ?requirementGroup .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    const population = queryResult.results.bindings[0].population.value;
    const drawingRight = queryResult.results.bindings[0].drawingRight.value;
    return {
      population,
      drawingRight
    };
  }

  return {
    population: 1000000,
    drawingRight: 1000000
  };
}
