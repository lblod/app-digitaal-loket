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
import electedPersonsReport from './electedPersonsReport';
import climateSubsidieOproep2PactsReport from './climateSubsidieOproep2PactsReport';
import climateSubsidie2PactsReport from './climateSubsidie2PactsReport';
import climateSubsidieOpvolgmomentReport from './climateSubsidieOpvolgmomentReport';
import gemeentewegenReport from './gemeentewegenReport';
import fietsSubsidieRequestsReport from './fietsSubsidieRequestsReport';
import fietsSubsidieBalanceRequestsReport from './fietsSubsidieBalanceRequestsReport';
import toezichtSubmissionsReport from './toezicht-submissions-report';
import toezichtTaxRegulationSubmissionReport from './toezicht-tax-regulation-submissions-report';
import ukraineSubsidyOproepOneReport from './ukraineSubsidyOproep1Report';
import linksBetweenWorshipServicesAndAdminUnitsReport from './links-between-worship-services-and-admin-units-report';
import virusScanReport from './virusScanReport';
import emailsInFailboxWithin24Hours from './emailsInFailboxWithin24hoursReport';

//Worship reports
import bedienaren from './worship/bedienaren';
import mandatarissen from './worship/mandatarissen';
import tempDeletes from './worship/temp-deletes';
import tempInserts from './worship/temp-inserts';

//Berichten reports
import berichten from './berichten/messages-with-provenance';

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
  electedPersonsReport,
  climateSubsidieOproep2PactsReport,
  climateSubsidie2PactsReport,
  climateSubsidieOpvolgmomentReport,
  gemeentewegenReport,
  fietsSubsidieRequestsReport,
  fietsSubsidieBalanceRequestsReport,
  // submissionsReport, TODO: disabled in favor of more optimised "toezicht-module-report"
  toezichtSubmissionsReport,
  toezichtTaxRegulationSubmissionReport,
  ukraineSubsidyOproepOneReport,
  linksBetweenWorshipServicesAndAdminUnitsReport,
  virusScanReport,
  emailsInFailboxWithin24Hours,

  //Worship reports
  bedienaren,
  mandatarissen,
  tempDeletes,
  tempInserts,

  //Berichten reports
  berichten,
];
