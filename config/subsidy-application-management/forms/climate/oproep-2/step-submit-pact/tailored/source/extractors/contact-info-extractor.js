const URI_BASE = "http://data.lblod.info/form-data/nodes/";

module.exports = {
  name: "climate-subsidy/submit-pact/contact-info-extractor",
  execute: async (store, graphs, lib, target, source) => {
    if (!source) source = target;

    const { $rdf, mu, sudo } = lib;

    const SCHEMA = new $rdf.Namespace("http://schema.org/");
    const FOAF = new $rdf.Namespace("http://xmlns.com/foaf/0.1/");

    const contactPoint = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add(
      $rdf.sym(target.uri),
      SCHEMA("contactPoint"),
      $rdf.sym(contactPoint),
      graphs.additions
    );

    const { firstName, familyName, email, telephone } = await getGenericInfo(
      source.uri,
      mu,
      sudo
    );

    if (familyName)
      store.add(
        $rdf.sym(contactPoint),
        FOAF("familyName"),
        familyName.value,
        graphs.additions
      );
    if (firstName)
      store.add(
        $rdf.sym(contactPoint),
        FOAF("firstName"),
        firstName.value,
        graphs.additions
      );
    if (email)
      store.add(
        $rdf.sym(contactPoint),
        SCHEMA("email"),
        email.value,
        graphs.additions
      );
    if (telephone)
      store.add(
        $rdf.sym(contactPoint),
        SCHEMA("telephone"),
        telephone.value,
        graphs.additions
      );
  },
};

async function getGenericInfo(uri, mu, sudo) {
  const { results } = await sudo.querySudo(`
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX schema: <http://schema.org/>
    PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT DISTINCT ?firstName ?familyName ?email ?telephone
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(uri)}
          schema:contactPoint ?contactPoint.
        OPTIONAL {
            ?contactPoint foaf:familyName ?familyName.
        }
        OPTIONAL {
            ?contactPoint foaf:firstName ?firstName.
        }
        OPTIONAL {
            ?contactPoint schema:email ?email.
        }
        OPTIONAL {
            ?contactPoint schema:telephone ?telephone.
        }
      }
    }`);

  if (results.bindings.length) {
    return results.bindings[0];
  }
  return null;
}
