const FORM_DATA_URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'bike-subsidy/request-balance/bank-info-extractor',
  execute: async (store, graphs, lib, target, source = null) => {

    if (!source)
      source = target;

    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SCHEMA =
        new $rdf.Namespace('http://schema.org/');
    const DCT =
        new $rdf.Namespace('http://purl.org/dc/terms/');
    const RDF =
        new $rdf.Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#');

    const bankAccount = new $rdf.NamedNode(FORM_DATA_URI_BASE + mu.uuid());

    store.add(
        $rdf.sym(target.uri),
        SCHEMA('bankAccount'),
        $rdf.sym(bankAccount),
        graphs.additions);

    store.add(
        $rdf.sym(bankAccount),
        RDF_TYPE,
        SCHEMA('BankAccount'),
        graphs.additions);

    const {iban} = await getGenericInfo(source.uri, mu, sudo);

    if (iban)
      store.add(
          $rdf.sym(bankAccount),
          SCHEMA('identifier'),
          iban.value,
          graphs.additions);
  },
};

async function getGenericInfo(uri, mu, sudo) {
  const {results} = await sudo.querySudo(`PREFIX schema: <http://schema.org/>

SELECT DISTINCT ?iban
WHERE {
  GRAPH ?g {
    ${mu.sparqlEscapeUri(uri)}
      schema:bankAccount ?bankAccount.   
    OPTIONAL { 
       ?bankAccount schema:identifier ?iban.
    }
  }
}`);

  if (results.bindings.length) {
    return results.bindings[0];
  }
  return null;
}