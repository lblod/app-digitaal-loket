const URI_BASE = 'http://data.lblod.info/form-data/nodes/';

module.exports = {
  name: 'climate/bicycle-infrastructure/request/missing-types-extractor',
  execute: async (store, graphs, lib, source) => {
    const {$rdf, mu, sudo} = lib;

    const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const SCHEMA = new $rdf.Namespace('http://schema.org/');
    const DECISION_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/decision-upload/');
    const REPORT_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/report-upload/');
    const AWARD_REPORT_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/award-report-upload/');
    const ACCOUNTABILITY_NOTE_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/accountability-note-upload/');
    const JUSTIFICATION_COSTS_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/justification-costs-upload/');
    const JUSTIFICATION_EXPROPRIATIONS_UPLOAD = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure/justification-expropriations-upload/');

    const bankAccount = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const decisionUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const reportUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const awardReportUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const accountabilityNoteUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const justificationCostsUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());
    const justificationExpropriationsUpload = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add($rdf.sym(source.uri), SCHEMA('bankAccount'), $rdf.sym(bankAccount), graphs.additions);
    store.add($rdf.sym(bankAccount), RDF_TYPE, SCHEMA('BankAccount'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('decisionUpload'), $rdf.sym(decisionUpload), graphs.additions);
    store.add($rdf.sym(decisionUpload), RDF_TYPE, DECISION_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('reportUpload'), $rdf.sym(reportUpload), graphs.additions);
    store.add($rdf.sym(reportUpload), RDF_TYPE, REPORT_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('awardReportUpload'), $rdf.sym(awardReportUpload), graphs.additions);
    store.add($rdf.sym(awardReportUpload), RDF_TYPE, AWARD_REPORT_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('accountabilityNoteUpload'), $rdf.sym(accountabilityNoteUpload), graphs.additions);
    store.add($rdf.sym(accountabilityNoteUpload), RDF_TYPE, ACCOUNTABILITY_NOTE_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('justificationCostsUpload'), $rdf.sym(justificationCostsUpload), graphs.additions);
    store.add($rdf.sym(justificationCostsUpload), RDF_TYPE, JUSTIFICATION_COSTS_UPLOAD('FormData'), graphs.additions);

    store.add($rdf.sym(source.uri), SUBSIDIE('justificationExpropriationsUpload'), $rdf.sym(justificationExpropriationsUpload), graphs.additions);
    store.add($rdf.sym(justificationExpropriationsUpload), RDF_TYPE, JUSTIFICATION_EXPROPRIATIONS_UPLOAD('FormData'), graphs.additions);
  }
}