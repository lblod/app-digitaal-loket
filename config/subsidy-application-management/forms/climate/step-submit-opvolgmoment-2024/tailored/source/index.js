const contactInfoExtractor = require('./extractors/contact-info-extractor');
const missingTypesExtractor = require('./extractors/missing-types-extractor');

const extractor = {
  name: 'climate-subsidy/opvolgmoment/first-step-as-source',
  execute
};

async function execute( store, graphs, lib, form ){
    const source = await findFormFromStep1(lib, form);

    //Note: this loop is still overkill, but at least it is 'prepared'
    for (let extractor of [ contactInfoExtractor ]) {
      try {
        await extractor.execute(store, graphs, lib, form, { uri: source });
      } catch (e) {
        console.warn(`Failed to execute inner extractor for ${extractor.name}`);
        console.log(e);
      }
    }
}

async function findFormFromStep1( { mu, sudo }, form ) {
  const queryStr = `
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>

    SELECT DISTINCT ?firstForm where {
     GRAPH ?g {
       ${mu.sparqlEscapeUri(form.uri)} dct:isPartOf ?currentStep .
       ?smc dct:source ${mu.sparqlEscapeUri(form.uri)} .
       ?firstForm dct:isPartOf ?step1 .
       ?smc dct:source ?firstForm .
     }
     GRAPH ?h {
       ?currentStep xkos:previous ?step3 .
       ?step3 xkos:previous ?step2 .
       ?step2 xkos:previous ?step1.
     }
   }`;

  const { results } = await sudo.querySudo(queryStr);

  if (!results.bindings.length)
    throw `Failed to find a preciding form for <${form.uri}>, are you sure this is part of a flow?`;

  return results.bindings[0].firstForm.value;
}

module.exports = [ extractor, missingTypesExtractor ];
