import automaticSubmission from './automatic-submission';
import berichtencentrumMelding from './berichtencentrum-melding';
import deltaProducerDumpFilePublisher from './delta-producer-dump-file-publisher';
import deltaProducerPublicationGraphMaintainer from './delta-producer-publication-graph-maintainer';
import deltaProducerReportGenerator from './delta-producer-report-generator';
import downloadUrl from './download-url';
import enrichSubmission from './enrich-submission';
import errorAlert from './error-alert';
import importSubmission from './import-submission';
import jobsController from './jobs-controller';
import prepareSubmissionForExport from './prepare-submission-for-export';
import resource from './resource';
import search from './search';
import syncWithKalliopeErrorNotification from './sync-with-kalliope-error-notification';
import toezichtFlattenedFormDataGenerator from './toezicht-flattened-form-data-generator';
import uuidGeneration from './uuid-generation';
import validateSubmission from './validate-submission';
import dispatcherWorshipMandates from './dispatcher-worship-mandates';
import vendorDataDistribution from './vendor-data-distribution';
import virusScanner from './virus-scanner';

export default [
  ...automaticSubmission,
  ...berichtencentrumMelding,
  ...deltaProducerDumpFilePublisher,
  ...deltaProducerPublicationGraphMaintainer,
  ...deltaProducerReportGenerator,
  ...downloadUrl,
  ...enrichSubmission,
  ...errorAlert,
  ...importSubmission,
  ...jobsController,
  ...prepareSubmissionForExport,
  ...resource,
  ...search,
  ...syncWithKalliopeErrorNotification,
  ...toezichtFlattenedFormDataGenerator,
  ...uuidGeneration,
  ...validateSubmission,
  ...dispatcherWorshipMandates,
  ...vendorDataDistribution,
  ...virusScanner
];
