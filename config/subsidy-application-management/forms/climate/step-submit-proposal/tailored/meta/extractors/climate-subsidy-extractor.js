module.exports = {
  name: 'climate-subsidy-extractor',
  execute: async (store, graphs, lib, form) => {
    console.log("BONJOUUUUUUUUUUUUUUUUUUUUUUUUUUUR");
    const { $rdf, mu, sudo } = lib;
    const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const DBPEDIA = new $rdf.Namespace('http://dbpedia.org/ontology/');

    console.log('ON CALCUUUUUUUUUUUUUUULE')
    const { population, drawingRight } = await getBestuurInfo(form.uri, mu, sudo);
    console.log(population, drawingRight);

    if (population && drawingRight) {
      store.add($rdf.sym(form.uri), DBPEDIA('populationTotal'), population, graphs.additions);
      store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('drawingRight'), drawingRight, graphs.additions);
    }

  }

};

// -------------------------------- INTERNAL LOGIC --------------------------------

async function getBestuurInfo(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX pav: <http://purl.org/pav/>
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX dbpedia: <http://dbpedia.org/ontology/>
    PREFIX m8g: <http://data.europa.eu/m8g/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>

    SELECT ?population ?drawingRight WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)} pav:createdBy ?bestuurseenheid .

        ?requirementGroup
          m8g:hasCriterionRequirement ?criterionRequirementBestuurseenheid ;
          m8g:hasCriterionRequirement ?criterionRequirementPopulation .

        ?criterionRequirementBestuurseenheid lblodSubsidie:isSatisfiableBy ?bestuurseenheid .

        ?criterionRequirementPopulation dbpedia:populationTotal ?population .
        ?subsidyAmount dct:requires ?requirementGroup ;
          subsidie:maximumbedrag ?drawingRight .
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

  return null;
}
