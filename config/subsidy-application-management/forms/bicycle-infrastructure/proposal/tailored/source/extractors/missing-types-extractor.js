const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'climate/bicycle-infrastructure/proposal/missing-types-extractor',
  execute: async (store, graphs, lib, source) => {
    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SCHEMA = new $rdf.Namespace('http://schema.org/');
    const SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const DECISION_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/decision-upload/');
    const PICTURES_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/pictures-upload/');

    const contactPoint = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const decisionUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const picturesUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add($rdf.sym(source.uri), SCHEMA('contactPoint'), $rdf.sym(contactPoint), graphs.additions);
    store.add($rdf.sym(contactPoint), RDF_TYPE, SCHEMA('ContactPoint'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('decisionUpload'), $rdf.sym(decisionUpload), graphs.additions);
    store.add($rdf.sym(decisionUpload), RDF_TYPE, DECISION_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('picturesUpload'), $rdf.sym(picturesUpload), graphs.additions);
    store.add($rdf.sym(picturesUpload), RDF_TYPE, PICTURES_UPLOAD('FormData'), graphs.additions);
  }
}