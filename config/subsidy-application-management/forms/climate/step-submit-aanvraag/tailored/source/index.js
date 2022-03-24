const contactInfoExtractor = require('./extractors/contact-info-extractor');

/**
 * NOTE: order of execution bound to position in the array
 */

firstFormExtractor = (extractors) =>
  Object.assign(
    {
      name: 'climate-subsidy/opvolgmoment/first-step-as-source',
      execute: async (store, graphs, lib, form) => {
        const {mu, sudo} = lib;

        const { results } = await sudo.querySudo(`
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
            ?currentStep xkos:previous ?step2 .
            ?step2 xkos:previous ?step1.
          }
        }`);

        if (!results.bindings.length)
          throw `Failed to find a preciding form for <${form.uri}>, are you sure this is part of a flow?`;

        const source = results.bindings[0].firstForm.value;
        for (let extractor of extractors) {
          try {
            await extractor.execute(
                store,
                graphs,
                lib,
                form,
                {uri: source});
          } catch (e) {
            console.warn(
                `Failed to execute inner extractor for ${extractor.name}`);
            console.log(e);
          }
        }
      }
    }
  )
module.exports = [
  firstFormExtractor([contactInfoExtractor]),
]
