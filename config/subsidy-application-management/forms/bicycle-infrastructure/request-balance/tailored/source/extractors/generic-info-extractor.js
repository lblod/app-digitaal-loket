module.exports = {
  name: 'bike-subsidy/request-balance/generic-info-extractor',
  execute: async (store, graphs, lib, target, source) => {

    if (!source)
      source = target;

    const {$rdf, mu, sudo} = lib;

    const LBLOD_SUBSIDIE = new $rdf.Namespace(
        'http://lblod.data.gift/vocabularies/subsidie/');
    const NIE = new $rdf.Namespace(
        'http://www.semanticdesktop.org/ontologies/2007/01/19/nie#');

    const info = await getGenericInfo(source.uri, mu, sudo);

    if (info) {
      const {projectName, identifier} = info;
      store.add(
          $rdf.sym(target.uri),
          LBLOD_SUBSIDIE('projectName'),
          projectName.value,
          graphs.additions);
      store.add(
          $rdf.sym(target.uri),
          NIE('identifier'),
          identifier.value,
          graphs.additions);
    }
  },
};

async function getGenericInfo(uri, mu, sudo) {
  const {results} = await sudo.querySudo(`PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>

SELECT DISTINCT ?projectName ?identifier
WHERE {
  GRAPH ?g {
    ${mu.sparqlEscapeUri(source.uri)}
      lblodSubsidie:projectName ?projectName ;
      nie:identifier ?identifier .
  }
}`);

  if (results.bindings.length) {
    return results.bindings[0];
  }
  return null;
}