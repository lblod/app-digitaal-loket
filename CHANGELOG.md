# Changelog
## 1.69.0 (2022-10-03)
- Release LPDC-module (both fronted and backend)
- Backend maintenance:
   - bump mu-auth
   - bump publication graph maintainer
- Update data erediensten
  - codelists OP
  - link person and nationality
## 1.68.6 (2022-09-27)
- propgate persons first or last names to all the graphs it belongs to
## 1.68.5 (2022-09-27)
- update bicycle subsidy deadline
## 1.68.4 (2022-09-26)
- added e-inclusion report
## 1.68.3 (2022-09-16)
- fix files graph persons sensitive
## 1.68.2 (2022-09-15)
- fix bestuursperiodeselector
## 1.68.1 (2022-09-13)
- bump download url service (to fix issues basic-auth issues for automatic-submission)
## 1.68.0 (2022-09-05)
### subsidies
- climate subsidies
- e-inclusion subsidies
### lpdc
- API + data + backend services
### erediensten
- API + data
### frontend
- erediensten mandatees (hidden feature flag)
- lpdc (hidden feature flag)
- various maintenance updates
### various
- upgrade identifier
- bump publication graph maintainers
## 1.67.0 (2022-08-05)
- update sync with kalliope: make sure timezone data is taken into account
- subsidy climate 1.0: adaptations
- subsidy Oekraïne: nooddorpen
- subsidy Oekraïne: nooddorpen
- maintenance docker-compose.dev.yml
- mandaten module: rangorde fix
- update automatic-submission related services: CBE should be able to send data
## 1.66.0 (2022-07-11)
- climate-subsidies: call 2
  - Pre-fill data.
  - Reporting
- Cleanup double uuids personen and related
- Frontend: new version semantic forms
- Bugfix berichtencentrum-sync-with-kalliope-service
  - correct dates zitting
- new bestuurseenheid: OCMW De Wijngaard
## 1.65.2 (2022-06-26)
- vandenbroele as erediensten
## 1.65.1 (2022-06-25)
- fix data issue double person
- deactivate plan samenleven subsidy
## 1.65.0 (2022-06-16)
- Frontend updates: incremental updates/bugfixes and new components
- Prepare subsidies Oekraine (temporarily offline)
- Subsidy-applications-management service: new JS template
- Forms erediensten
- Klimaatsubsidies oproep 2
- GZG oproep 6 + 7 and reports
- GZG oproep 6 + 7 extra pick-list entries
- plan samenleven: update deadline
- Bicycle subsidies: AGB should be able to fetch these
## 1.64.4 (2022-04-26)
- hotfix: change subsidy gzg deadline + codelists
## 1.64.3 (2022-04-26)
- hotfix: show reports in concept for noodopvang
- hotfix: change deadline plan samenleven
- hotfix: fix/workaround performance persons sensitive
## 1.64.2 (2022-04-26)
 - bumped identifier
## 1.64.1 (2022-04-25)
 - tweaking cronjob setting
## 1.64.0 (2022-04-19)
### :rocket: Enhancement
 - subsidies plan samenleven deltas
 - subsidies plan samenleven
 - subsidies plan samenleven report
 - person-sensitive data deltas
### :bug: Bug Fix
 - BBCDR fix files list
### :house: Internal
 - refactor a bit authorization of deltas
 - lots of improvements in ember-submission-form-fields: deduplication, bugfixes, new components
## 1.63.0 (2022-03-31)
### :house: Internal
  - simplified producers: remove json-diff-file-producers (since provided by other services)
#### subsidies
  - update deadline climate/oproep-1/opvolgmoment
  - added plan samenleven
## 1.62.0 (2022-03-25)
#### subsidies
  - Update bike subsidy: added extra file upload fields
  - extend filtering beschikbare subsidiemaatregelen: not all types of eenheden can now submit for all subsidies
  - update labeling climate subsidy
### :house: Internal
  - huge frontend maintenance: removal of webuniversum dependencies, update ember and other maintenance
  - update publication graph maintainer (some fixes/improvements)
## 1.61.0 (2022-03-10)
#### erediensten
  - adding new bestuurseenheden
#### subsidies
  - New GZG Oproep 5 reeks + report
## 1.60.1 (2022-03-07)
### :house: Internal
  - update export submissions config, to include missing besluitenlijsten
## 1.60.0 (2022-03-01)
### :house: Internal
  - update export submissions config, to include missing besluitenlijsten
## 1.59.1 (2022-03-01)
### :house: Internal
  - update publicaiton graph maintainer config
## 1.59.0 (2022-02-25)
### :house: Internal
 - Improved reports MAR-code
 - clean up database for obsolete information
 - Introduce config file berichtencentrum sync with kalliope
### :rocket: Enhancement
- New inzendingen voor toezicht:
  - Collective notie van wantrouwen
  - Code goed bestuur
  - Overzicht presentiegelden
  - Voorstellen in verband saneringsplan
## 1.58.7 (2022-02-11)
### :house: Internal
- cleanup double subsidy bierbeek climate
## 1.58.6 (2022-02-11)
### :rocket: Enhancement
- extend deadline gzg subisdy oproep 4
## 1.58.5 (2022-02-11)
### :memo: Reports
- **[NEW]** Toezicht module:
  - Produces two CSVs in the dashboard:
    - Meldingen
    - Geuploade bestanden
- **[DEPRECATED]** Submissions
### :rocket: Enhancement
- Bumped publication-graph-maintainer to latest
## 1.58.4 (2022-02-08)
### :busts_in_silhouette: Gebruikers
- **[NEW]** OCMW vereniging: Welzijnsvereniging Ter Nethe
- **[NEW]** OCMW vereniging: De Zilveren Zwaan
## 1.58.3 (2022-02-4)
### :memo: Reports
- Submission reports now include a column for publication-date
## 1.58.2 (2022-02-4)
- Fix authorization rule for subsidies: didn't have access to the files
## 1.58.1 (2022-01-27)
- Extend the period of subsidy GzG - callup 4
## 1.58.0 (2022-01-25)
### :house: Internal
 - Re-organise migration files
 - Update performance of delta-publication flow for submissions (a separate triple store has been introduced)
### :rocket: Enhancement
 - Included forms vastelling gemeentelijk beleidskader
## 1.57.3 (2022-01-23)
### :house: Internal
 - Update file service to deal with performance issues of files deletion residing in multiple graphs
## 1.57.2 (2022-01-21)
### :house: Internal
 - fixed missing migration: move attachments of manually created submissions
## 1.57.1 (2022-01-21)
### :house: Internal
 - disable the internal mandaten report, because performance concerns
## 1.57.0 (2022-01-21)
### :house: Internal
 - move files to org-graphs: migrations and update corresponding services to handle new location
 - subsidy-applications-managenent: improve and simplify code tailored-data
### :rocket: Enhancement
- new bestuurseenheid: Politie Deinze-Zulte-Lievegem
#### subsidies
- Noodopvang Kerst 2012:
  - 7 new series forms
  - reports
- Fietsinfrastructuur: new step + reports
- Reports GzG operoep 3 and 4

## 1.56.0 (2021-12-23)
### :rocket: Enhancement
- Update report contactracing: added nazomer.

## 1.55.0 (2021-12-23)
### :rocket: Enhancement
- New report GZG: oproep 2
- Update report contactracing: KBO-number

## 1.54.0 (2021-12-21)
### :rocket: Enhancement
- Create publication producer: this will make sure submissions can be consumed by app-public-decisions
- Improve file-download-service: allow file type guessing if none is provided. (mainly to tackle VGC case)

### :house: Internal
- Validate-submission: less agressive rescheduling of remote-dataobjects, when submitting.
- Bump mu-java-script-template in multiple services, as to handle edge cases for nasty URL's
- General performance improvements on the passing of delta's by using mu-scope-ids.
- Bump all producers

## 1.53.0 (2021-12-06)
### :rocket: Enhancement
- Subsidies: improve behaviour of delete button. Don't allow delete when submitting, saving or transition to next step.
- Subsidies: GzG oproep 4
- Upgrade virtuoso version to tenforce/virtuoso:1.3.2-virtuoso7.2.5.1

### :house: Internal
- Remove export leidinggevenden and mandaten, as these are covered by delta-producers
- Remove logging to app-http-logger on export-submissions. To decrease load.
- Bump automatic-submission, to have configurable endpoint to resolve json-ld endpoints.

## 1.52.0 (2021-11-11)

### :rocket: Enhancement
- Report GzG: include concept status
- General improvement reports: make less assumptions, so potential issues with forms become more visible.
- Update submissions report to show bestuursorgaan + performance improvement
- Update GzG oproep 3: deadline

## 1.51.0 (2021-11-10)

### :rocket: Enhancement
- GzG oproep 2 and 3
- Oproep contact tracing 'nazomer'

## 1.50.3 (2021-11-04)

### :ambulance: Hotfix
- Deleted a message in the berichtencentrum that wasn't intended for this bestuurseenheid

## 1.50.2 (2021-10-29)

### :ambulance: Hotfix
- Moved Climate subsidy-consumptions one step back per request of users, accidental submission by user

## 1.50.1 (2021-10-26)

### :ambulance: Hotfix
- Moved 2 Climate subsidy-consumptions one step back per request of users, accidental submission by users

## 1.50.0 (2021-10-25)

### :house: Internal
- Update email adresses for automatic submission
- Update delta-producer-report-generator to use error notifications

### :rocket: Enhancement
- GzG report subsidies
- Force status for mandataris

## 1.49.3 (2021-10-21)

### :ambulance: Hotfix
- Moved 2 bike subsidy-consumption one step back per request of a user, accidental submission by user

## 1.49.2 (2021-10-19)

### :ambulance: Hotfix
- Moved subsidy-consumption one step back per request of a user, accidental submission by user
- Removed identical subsidy-consumptions per request of a user

### :house: Internal
- Moved up submission-date for Climate Subsidy's Proposal to 2022-10-31

## 1.49.1 (2021-10-15)

### :bug: Bug Fix
- subsidy-application-flow-management-service: Wrong branch was released, new branch for new release
- Migrations to correct subsidies which have been harmed

## 1.49.0 (2021-10-14)

### :house: Internal
- introduce alert service to get more error notifications
- improve some subsidy reports
- automatic submission service should give us alerts

### :bug: Bug Fix
- subsidy-application-flow-management-service: uncaught error
- subsidy-application-management-service: return correct http error when inconsistent state

### :rocket: Enhancement
- Introduce super SSN request
- Improve attachmenet fetching: not only text/html should be fixed
- authentication for download-url

## 1.48.4 (2021-10-08)

### :house: Internal
- change deadline subsidy

## 1.48.3 (2021-10-08)

### :house: Internal
- unblock accidentally submitted form

## 1.48.2 (2021-09-28)

### :bug: Bug Fix
- prepared Loket to reconcile user.

## 1.48.1 (2021-09-27)

### :rocket: Enhancement
- sync-with-kalliope-service: improve error handling: added gracefully handling of non-connection type of errors.
- Fietssubsidies: Improve validation on table fietssubsidies: make sure some fields cannot be left empty

### :bug: Bug Fix
- escape helpers for sparqlEscape URI: don't escape `'`
- Fietssubsidies don't show empty table on transition to next step

## 1.48.0 (2021-09-16)

### :rocket: Enhancement
 - Adjustments GZG
 - Report for fietssubsidies
 - Download URL: consistent handling of file download streams
 - update deadline fietssubsidie

### :bug: Bug Fix
 - ACM/IDM: switch betuurseeneid needed updated after breaking changes ember-acm-idm
 - Fix empty susbsidy form: DL-3028

## 1.47.1 (2021-09-10)

### :bug: Bug Fix
 - step 2 fietssubsidies, some edge cases were not fixed in the activation of step 2.

## 1.47.0 (2021-09-03)

### :house: Internal
 - bumped frontend
 - addedd CHANGELOG.md

### :bug: Bug Fix
 - step 2 fietssubsidies should take over data from step 1 in all cases

### :rocket: Enhancement
 - harvesting attachtments in automatic submission flow
