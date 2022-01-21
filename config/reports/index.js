import BestuurseenhedenReport from './bestuurseenhedenReport';
import BerichtencentrumMessagesReport from './berichtencentrumMessages';
import InzendingenReport from './inzendingenReport';
import AccountReport from './accountReport';
import bbcdrReport from './bbcdrReport';
import sameRrnReport from './sameRrnReport';
import failedSyncToMft from './failedSyncToMft';
import bestuurseenhedenWithoutNotificationEmail from './bestuurseenhedenWithoutNotificationEmail';
import internalMandatenReport from './internalMandatenReport';
import submissionsReport from './submissionsReport';
import personenWithMultipleNamesReport from './personenWithMultipleNamesReport';
import mandatarissenWithMultipleStartDateReport from './mandatarissenWithMultipleStartDateReport';
import mandatarissenWithMultipleEndDateReport from './mandatarissenWithMultipleEndDateReport';
import mandatarissenWithoutStartDateReport from './mandatarissenWithoutStartDateReport';
import personsMissingDataReport from './personsMissingDataReport';
import mandatarissenWithoutPersonReport from './mandatarissenWithoutPersonReport';
import mandatarissenWithEmptyPersonReport from './mandatarissenWithEmptyPersonReport';
import noodopvangSubsidiesReport from './noodopvangSubsidiesReport';
import mar7300InzendingenReport from './mar7300InzendingenReport';
import contactTracingSubsidiesReport from './contactTracingSubsidiesReport';
import electedPersonsReport from './electedPersonsReport';
import contactTracingSubsidiesDetailedReport from './contactTracingSubsidiesDetailedReport';
import climateSubsidiePactsReport from './climateSubsidiePactsReport';
import gemeentewegenReport from './gemeentewegenReport';
import gzgSubsidieOproepOneReport from './gzgSubsidieOproep1Report';
import gzgSubsidieOproepTwoReport from './gzgSubsidieOproep2Report';
import gzgSubsidieOproepThreeReport from './gzgSubsidieOproep3Report';
import gzgSubsidieOproepFourReport from './gzgSubsidieOproep4Report';
import fietsSubsidieProposalsReport from './fietsSubsidieProposalsReport';
import fietsSubsidieRequestsReport from './fietsSubsidieRequestsReport';
import fietsSubsidieBalanceRequestsReport from './fietsSubsidieBalanceRequestsReport';

export default [
  BestuurseenhedenReport,
  BerichtencentrumMessagesReport,
  InzendingenReport,
  AccountReport,
  bbcdrReport,
  sameRrnReport,
  failedSyncToMft,
  bestuurseenhedenWithoutNotificationEmail,
  //internalMandatenReport, TODO: since the introduction ofg the publication graph, this report became too heavy)
  submissionsReport,
  personenWithMultipleNamesReport,
  mandatarissenWithMultipleStartDateReport,
  mandatarissenWithMultipleEndDateReport,
  mandatarissenWithoutStartDateReport,
  personsMissingDataReport,
  mandatarissenWithoutPersonReport,
  mandatarissenWithEmptyPersonReport,
  noodopvangSubsidiesReport,
  mar7300InzendingenReport,
  contactTracingSubsidiesReport,
  electedPersonsReport,
  contactTracingSubsidiesDetailedReport,
  climateSubsidiePactsReport,
  gemeentewegenReport,
  fietsSubsidieProposalsReport,
  fietsSubsidieRequestsReport,
  fietsSubsidieBalanceRequestsReport,
  gzgSubsidieOproepOneReport,
  gzgSubsidieOproepTwoReport,
  gzgSubsidieOproepThreeReport,
  gzgSubsidieOproepFourReport,
];
