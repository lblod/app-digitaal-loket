const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'bike-subsidy/request-balance/bank-info-extractor',
  execute: async (store, graphs, lib, target, source = null) => {

    if (!source)
      source = target;

    const {$rdf, mu, sudo} = lib;

    const SCHEMA =
        new $rdf.Namespace('http://schema.org/');
    const DCT =
        new $rdf.Namespace('http://purl.org/dc/terms/');
    const RDF =
        new $rdf.Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#');

    const bankAccount = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add(
        $rdf.sym(target.uri),
        SCHEMA('bankAccount'),
        $rdf.sym(bankAccount),
        graphs.additions);

    const {iban} = await getGenericInfo(source.uri, mu, sudo);

    if (iban)
      store.add(
          $rdf.sym(bankAccount),
          SCHEMA('identifier'),
          iban.value,
          graphs.additions);

    const letters = await getConfirmationLetters(source.uri, mu, sudo);

    if (letters && letters.length) {
      letters.forEach(({uri, type}) => {
        store.add(
            $rdf.sym(bankAccount),
            DCT('hasPart'),
            $rdf.sym(uri.value),
            graphs.additions);
        store.add(
            $rdf.sym(uri),
            RDF('type'),
            $rdf.sym(type.value),
            graphs.additions);
      });
    }
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

async function getConfirmationLetters(uri, mu, sudo) {
  const {results} = await sudo.querySudo(`PREFIX schema: <http://schema.org/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT DISTINCT ?letter ?type
WHERE {
    ${mu.sparqlEscapeUri(uri)}
      schema:bankAccount ?bankAccount.
    ?bankAccount
        dct:hasPart ?letter.
    ?letter
        rdf:type ?type.
}`);

  if (results.bindings.length) {
    return results.bindings.map(binding => Object.assign({
      uri: binding.letter,
      type: binding.type,
    }));
  }
  return null;
}