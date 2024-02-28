const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'climate/bicycle-infrastructure/proposal/missing-types-extractor',
  execute: async (store, graphs, lib, source) => {
    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const PICTURE_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/pictures-upload/');
    const INVOICE_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/invoice-upload/');
    const REPORT_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/report-upload/');

    const pictureUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const invoiceUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const reportUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add($rdf.sym(source.uri), SUBSIDIE('picturesUpload'), $rdf.sym(pictureUpload), graphs.additions);
    store.add($rdf.sym(pictureUpload), RDF_TYPE, PICTURE_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('invoiceUpload'), $rdf.sym(invoiceUpload), graphs.additions);
    store.add($rdf.sym(invoiceUpload), RDF_TYPE, INVOICE_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('reportUpload'), $rdf.sym(reportUpload), graphs.additions);
    store.add($rdf.sym(reportUpload), RDF_TYPE, REPORT_UPLOAD('FormData'), graphs.additions);

  }
}