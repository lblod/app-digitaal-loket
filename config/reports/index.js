import BestuurseenhedenReport from './bestuurseenhedenReport';
import BerichtencentrumMessagesReport from './berichtencentrumMessages';
import InzendingenReport from './inzendingenReport';
import AccountReport from './accountReport';
import bbcdrReport from './bbcdrReport';
import sameRrnReport from './sameRrnReport';
import failedSyncToMft from './failedSyncToMft';
import bestuurseenhedenWithoutNotificationEmail from './bestuurseenhedenWithoutNotificationEmail';
import personenWithMultipleNamesReport from './personenWithMultipleNamesReport';
import mandatarissenWithMultipleStartDateReport from './mandatarissenWithMultipleStartDateReport';
import mandatarissenWithMultipleEndDateReport from './mandatarissenWithMultipleEndDateReport';
import mandatarissenWithoutStartDateReport from './mandatarissenWithoutStartDateReport';
import personsMissingDataReport from './personsMissingDataReport';
import mandatarissenWithoutPersonReport from './mandatarissenWithoutPersonReport';
import mandatarissenWithEmptyPersonReport from './mandatarissenWithEmptyPersonReport';
import noodopvangSubsidiesReport from './noodopvangSubsidiesReport';
import contactTracingSubsidiesReport from './contactTracingSubsidiesReport';
import electedPersonsReport from './electedPersonsReport';
import contactTracingSubsidiesDetailedReport from './contactTracingSubsidiesDetailedReport';
import climateSubsidiePactsReport from './climateSubsidiePactsReport';
import climateSubsidieOproep2PactsReport from './climateSubsidieOproep2PactsReport';
import gemeentewegenReport from './gemeentewegenReport';
import gzgSubsidieOproepOneReport from './gzgSubsidieOproep1Report';
import gzgSubsidieOproepTwoReport from './gzgSubsidieOproep2Report';
import gzgSubsidieOproepThreeReport from './gzgSubsidieOproep3Report';
import gzgSubsidieOproepFourReport from './gzgSubsidieOproep4Report';
import gzgSubsidieOproepFiveReport from './gzgSubsidieOproep5Report';
import gzgSubsidieOproepSixReport from './gzgSubsidieOproep6Report';
import gzgSubsidieOproepSevenReport from './gzgSubsidieOproep7Report';
import fietsSubsidieProposalsReport from './fietsSubsidieProposalsReport';
import fietsSubsidieRequestsReport from './fietsSubsidieRequestsReport';
import fietsSubsidieBalanceRequestsReport from './fietsSubsidieBalanceRequestsReport';
import toezichtSubmissionsReport from './toezicht-submissions-report';
import toezichtTaxRegulationSubmissionReport from './toezicht-tax-regulation-submissions-report';
import planSubsidieProposalsReport from './planSubsidieProposalsReport';
import ukraineSubsidyOproepOneReport from './ukraineSubsidyOproep1Report';

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
  personenWithMultipleNamesReport,
  mandatarissenWithMultipleStartDateReport,
  mandatarissenWithMultipleEndDateReport,
  mandatarissenWithoutStartDateReport,
  personsMissingDataReport,
  mandatarissenWithoutPersonReport,
  mandatarissenWithEmptyPersonReport,
  noodopvangSubsidiesReport,
  contactTracingSubsidiesReport,
  electedPersonsReport,
  contactTracingSubsidiesDetailedReport,
  climateSubsidiePactsReport,
  climateSubsidieOproep2PactsReport,
  gemeentewegenReport,
  fietsSubsidieProposalsReport,
  fietsSubsidieRequestsReport,
  fietsSubsidieBalanceRequestsReport,
  gzgSubsidieOproepOneReport,
  gzgSubsidieOproepTwoReport,
  gzgSubsidieOproepThreeReport,
  gzgSubsidieOproepFourReport,
  gzgSubsidieOproepFiveReport,
  gzgSubsidieOproepSixReport,
  gzgSubsidieOproepSevenReport,
  // submissionsReport, TODO: disabled in favor of more optimised "toezicht-module-report"
  toezichtSubmissionsReport,
  toezichtTaxRegulationSubmissionReport,
  planSubsidieProposalsReport,
  ukraineSubsidyOproepOneReport
];
