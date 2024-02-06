const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'climate/step-submit-pact/missing-types-extractor',
  execute: async (store, graphs, lib, source) => {
    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SCHEMA = new $rdf.Namespace('http://schema.org/');
    const SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const SIGNED_PACT = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/climate/signed-pact/');


    const contactPoint = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const signedPact = new $rdf.NamedNode(URI_BASE + mu.uuid());


    store.add($rdf.sym(source.uri), SCHEMA('contactPoint'), $rdf.sym(contactPoint), graphs.additions);
    store.add($rdf.sym(contactPoint), RDF_TYPE, SCHEMA('ContactPoint'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('signedPact'), $rdf.sym(signedPact), graphs.additions);
    store.add($rdf.sym(signedPact), RDF_TYPE, SIGNED_PACT('FormData'), graphs.additions);
  }
}