import automaticSubmission from './automatic-submission';
import deltaProducerDumpFilePublisher from './delta-producer-dump-file-publisher';
import deltaProducerPublicationGraphMaintainerLeidinggevenden from './delta-producer-publication-graph-maintainer-leidinggevenden';
import deltaProducerPublicationGraphMaintainerMandatarissen from './delta-producer-publication-graph-maintainer-mandatarissen';
import deltaProducerPublicationGraphMaintainerPersonsSensitive from './delta-producer-publication-graph-maintainer-persons-sensitive';
import deltaProducerPublicationGraphMaintainerWSSensitive from './delta-producer-publication-graph-maintainer-ws-sensitive';
import deltaProducerPublicationGraphMaintainer from './delta-producer-publication-graph-maintainer';
import deltaProducerPublicationGraphMaintainerSubsidies from './delta-producer-publication-graph-maintainer-subsidies';
import deltaProducerPublicationGraphMaintainerWorshipSubmissions from './delta-producer-publication-graph-maintainer-worship-submissions';
import deltaProducerReportGenerator from './delta-producer-report-generator';
import downloadUrl from './download-url';
import enrichSubmission from './enrich-submission';
import errorAlert from './error-alert';
import importSubmission from './import-submission';
import jobsController from './jobs-controller';
import lpdcManagement from './lpdc-management';
import lpdcPublish from './lpdc-publish';
import prepareSubmissionForExport from './prepare-submission-for-export';
import resource from './resource';
import syncWithKalliopeErrorNotification from './sync-with-kalliope-error-notification';
import toezichtFlattenedFormDataGenerator from './toezicht-flattened-form-data-generator';
import validateSubmission from './validate-submission';
import deltaConsumerWorshipMandates from './delta-consumer-worship-services';
import vendorDataDistribution from './vendor-data-distribution';

export default [
  ...automaticSubmission,
  ...deltaProducerDumpFilePublisher,
  ...deltaProducerPublicationGraphMaintainerLeidinggevenden,
  ...deltaProducerPublicationGraphMaintainerMandatarissen,
  ...deltaProducerPublicationGraphMaintainerPersonsSensitive,
  ...deltaProducerPublicationGraphMaintainerWSSensitive,
  ...deltaProducerPublicationGraphMaintainer,
  ...deltaProducerPublicationGraphMaintainerSubsidies,
  ...deltaProducerPublicationGraphMaintainerWorshipSubmissions,
  ...deltaProducerReportGenerator,
  ...downloadUrl,
  ...enrichSubmission,
  ...errorAlert,
  ...importSubmission,
  ...jobsController,
  ...lpdcManagement,
  ...lpdcPublish,
  ...prepareSubmissionForExport,
  ...resource,
  ...syncWithKalliopeErrorNotification,
  ...toezichtFlattenedFormDataGenerator,
  ...validateSubmission,
  ...deltaConsumerWorshipMandates,
  ...vendorDataDistribution
];
