const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'climate/bicycle-infrastructure/proposal/missing-types-extractor',
  execute: async (store, graphs, lib, source) => {
    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SCHEMA = new $rdf.Namespace('http://schema.org/');
    const SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const ACCOUNTABILITY_SUMMARY= new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/ukraine-nooddorpen/accountability-summary/');
    const ACCOUNTABILITY_PROOF= new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/ukraine-nooddorpen/accountability-proof/');

    const contactPoint = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const accountabilitySummary = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const accountabilityProof = new $rdf.NamedNode(URI_BASE + mu.uuid());
    
    store.add($rdf.sym(source.uri), SCHEMA('contactPoint'), $rdf.sym(contactPoint), graphs.additions);
    store.add($rdf.sym(contactPoint), RDF_TYPE, SCHEMA('ContactPoint'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('accountabilitySummary'), $rdf.sym(accountabilitySummary), graphs.additions);
    store.add($rdf.sym(accountabilitySummary), RDF_TYPE, ACCOUNTABILITY_SUMMARY('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('accountabilityProof'), $rdf.sym(accountabilityProof), graphs.additions);
    store.add($rdf.sym(accountabilityProof), RDF_TYPE, ACCOUNTABILITY_PROOF('FormData'), graphs.additions);

  }
}