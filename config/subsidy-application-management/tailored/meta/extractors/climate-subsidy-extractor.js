module.exports = {
  name: 'climate-subsidy-extractor',
  execute: async (store, graphs, lib, form) => {

    const { $rdf, mu, sudo } = lib;
    const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const DBPEDIA = new $rdf.Namespace('http://dbpedia.org/ontology/');
    const CLIMATE_SUBSIDIE_MEASURE = "http://lblod.data.gift/concepts/5e52660c-9067-4d6f-b82c-c73ffd043401";

    const measureUri = await getMeasure(form.uri, mu, sudo);
    if (measureUri == CLIMATE_SUBSIDIE_MEASURE) {
      const { population, drawingRight } = await getBestuurInfo(form.uri, mu, sudo);

      if (population && drawingRight) {
        store.add($rdf.sym(form.uri), DBPEDIA('populationTotal'), population, graphs.additions);
        store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('drawingRight'), drawingRight, graphs.additions);
      }
    }
  }

};

// -------------------------------- INTERNAL LOGIC --------------------------------

async function getMeasure(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    SELECT ?measure WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)} lblodSubsidie:subsidyMeasure ?measure .
      }
    }
  `);

  if (queryResult.results.bindings.length)
    return queryResult.results.bindings[0].measure.value;

  return null;
}

async function getBestuurInfo(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX pav: <http://purl.org/pav/>
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX dbpedia: <http://dbpedia.org/ontology/>
    SELECT ?population ?drawingRight WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)} pav:createdBy ?bestuurseenheid .
        ?bestuurseenheid dbpedia:populationTotal ?population ;
          lblodSubsidie:drawingRight ?drawingRight .
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
