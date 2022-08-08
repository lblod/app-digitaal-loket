const contactInfoExtractor = require("./extractors/contact-info-extractor");
const pactFileExtractor = require("./extractors/pact-file-extractor");

const extractor = {
  name: "climate-subsidy/pact/previous-call-as-source",
  execute,
};

async function execute(store, graphs, lib, form) {
  const bestuurseenheidURI = await findBestuursEenheid(lib, form);
  const source = await findFormFromPreviousCall(lib, bestuurseenheidURI);

  //Note: this loop is still overkill, but at least it is 'prepared'
  for (let extractor of [contactInfoExtractor]) {
    try {
      await extractor.execute(store, graphs, lib, form, { uri: source });
    } catch (e) {
      console.warn(`Failed to execute inner extractor for ${extractor.name}`);
      console.log(e);
    }
  }

  //Note: this loop is still overkill, but at least it is 'prepared'
  for (let extractor of [pactFileExtractor]) {
    try {
      await extractor.execute(store, graphs, lib, form, { uri: source });
    } catch (e) {
      console.warn(`Failed to execute inner extractor for ${extractor.name}`);
      console.log(e);
    }
  }
}

async function findBestuursEenheid({ mu, sudo }, form) {
  const queryStr = `
    PREFIX m8g: <http://data.europa.eu/m8g/>
    PREFIX dct: <http://purl.org/dc/terms/>

    SELECT DISTINCT ?bestuurseenheidURI {
      ?subsidie dct:source ${mu.sparqlEscapeUri(form.uri)} ;
        m8g:hasParticipation ?participation.
      ?bestuurseenheidURI m8g:playsRole ?participation.
  }`

  const { results } = await sudo.querySudo(queryStr);

  if (!results.bindings.length)
    throw `Failed to find the bestuurseenheid for <${form.uri}>, are you sure this is part of a flow?`;

  return results.bindings[0].bestuurseenheidURI.value;

}

async function findFormFromPreviousCall({ mu, sudo }, bestuurseenheid) {
  const subsidyMeasureOffers = "http://lblod.data.info/id/subsidy-measure-offers/64d40351-8128-464f-990f-41066154583e";
  const step = "http://data.lblod.info/id/subsidy-procedural-steps/d6ec1fb1-a991-47ba-95f1-afdd87e4553c";
  const queryStr = `
    
PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
PREFIX m8g: <http://data.europa.eu/m8g/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX adms: <http://www.w3.org/ns/adms#>


SELECT DISTINCT ?form {
  ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
    transactie:isInstantieVan ${mu.sparqlEscapeUri(subsidyMeasureOffers)} ;
    dct:source ?form .
  ?form dct:isPartOf/dct:references ${mu.sparqlEscapeUri(step)};
    adms:status ?status.
  
  ?form dct:modified ?modified .
  
  ?subsidie m8g:hasParticipation ?participation.
  ?bestuurseenheidURI m8g:playsRole ?participation.
  
  FILTER(?bestuurseenheidURI in ( ${mu.sparqlEscapeUri(bestuurseenheid)} ) )

} ORDER BY DESC(?status) DESC(?modified) 
LIMIT 1`;

  const { results } = await sudo.querySudo(queryStr);

  if (!results.bindings.length)
    throw `Failed to find previously sent call for <${form.uri}> for this bestuurseenheid!`;

  return results.bindings[0].form.value;
}

module.exports = [extractor];
