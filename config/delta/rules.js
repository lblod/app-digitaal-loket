import automaticSubmission from './automatic-submission';
import berichtencentrumMelding from './berichtencentrum-melding';
import deltaProducerDumpFilePublisher from './delta-producer-dump-file-publisher';
import deltaProducerPublicationGraphMaintainer from './delta-producer-publication-graph-maintainer';
import deltaProducerPublicationGraphMaintainerSubsidies from './delta-producer-publication-graph-maintainer-subsidies';
import deltaProducerReportGenerator from './delta-producer-report-generator';
import downloadUrl from './download-url';
import enrichSubmission from './enrich-submission';
import errorAlert from './error-alert';
import importSubmission from './import-submission';
import jobsController from './jobs-controller';
import prepareSubmissionForExport from './prepare-submission-for-export';
import resource from './resource';
import syncWithKalliopeErrorNotification from './sync-with-kalliope-error-notification';
import toezichtFlattenedFormDataGenerator from './toezicht-flattened-form-data-generator';
import validateSubmission from './validate-submission';
import dispatcherWorshipMandates from './dispatcher-worship-mandates';
import vendorDataDistribution from './vendor-data-distribution';
import virusScanner from './virus-scanner';
import upsertMandatenbheerEntity from './upsert-mandatenbeheer-entity';

export default [
  ...automaticSubmission,
  ...berichtencentrumMelding,
  ...deltaProducerDumpFilePublisher,
  ...deltaProducerPublicationGraphMaintainer,
  ...deltaProducerPublicationGraphMaintainerSubsidies,
  ...deltaProducerReportGenerator,
  ...downloadUrl,
  ...enrichSubmission,
  ...errorAlert,
  ...importSubmission,
  ...jobsController,
  ...prepareSubmissionForExport,
  ...resource,
  ...syncWithKalliopeErrorNotification,
  ...toezichtFlattenedFormDataGenerator,
  ...validateSubmission,
  ...dispatcherWorshipMandates,
  ...vendorDataDistribution,
  ...virusScanner,
  ...upsertMandatenbheerEntity
];
