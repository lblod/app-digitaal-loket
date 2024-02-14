const genericInfo = require(
    './extractors/generic-info-extractor');
const contactInfo = require(
    './extractors/contact-info-extractor');
const bankInfo = require(
    './extractors/bank-info-extractor');
const missingTypesExtractor = require('./extractors/missing-types-extractor');

precedingFormExtractor = (extractors) =>
    Object.assign(
        {
          name: 'bike-subsidy/request-balance/previous-form-as-source',
          execute: async (store, graphs, lib, form) => {
            const {mu, sudo} = lib;
            const {results} = await sudo.querySudo(`PREFIX dct: <http://purl.org/dc/terms/>
PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>

SELECT DISTINCT ?previousForm where {
  GRAPH ?g {
    ${mu.sparqlEscapeUri(form.uri)} dct:isPartOf ?currentStep .
    ?smc dct:source ${mu.sparqlEscapeUri(form.uri)} .
    ?previousForm dct:isPartOf ?previousStep .
    ?smc dct:source ?previousForm .
  }
  GRAPH ?h {
    ?currentStep xkos:previous ?previousStep .
  }
}`);
            if (!results.bindings.length)
              throw `Failed to find a preciding form for <${form.uri}>, are you sure this is part of a flow?`;
            const source = results.bindings[0].previousForm.value;
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
          },
        },
    );

/**
 * NOTE: order of execution bound to position in the array
 */
module.exports = [
  precedingFormExtractor([genericInfo, contactInfo, bankInfo]), missingTypesExtractor,
];