# Changelog

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
