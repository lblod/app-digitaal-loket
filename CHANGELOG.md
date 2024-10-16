# Changelog
## unreleased
### LMB
 - cut-over to LMB: see DL-6144.
 - Bump package-bbcdr [DL-6193]. (It basically adds a `DISTINCT` to `SELECT` queries)
### Deploy notes
#### LMB public
In `docker-compose.override.yml`
```
  lmb-public-ldes-client:
    environment:
      LDES_BASE: "https://mandatenbeheer.lblod.info/streams/ldes/public/" # Adapt endpoint in function of environment.
      FIRST_PAGE: "https://mandatenbeheer.lblod.info/streams/ldes/public/1"
      BYPASS_MU_AUTH: "true"
```

```
drc down;
# flushing data first
# Please make sure the correct config file is used for the virtuoso, or it might just get stuck
cd scripts/purge-lmb-data/;
drc -f docker-compose.script.yml up -d # check logs until finishes
drc -f docker-compose.script.yml exec virtuoso bash;
isql-v;
exec('checkpoint');
exit;
exit;
drc -f docker-compose.script.yml down
cd -
drc up -d database virtuoso # wait for proper startup of virtuoso
drc up -d lmb-public-ldes-client # Wait until success
# Here comment out all delta-rules in (config/delta/rules.js) except the ones going to:
#  - delta-producer-publication-graph-maintainer
#  - delta-producer-dump-file-publisher
drc up -d database virtuoso deltanotifier resource delta-producer-background-jobs-initiator delta-producer-publication-graph-maintainer publication-triplestore delta-producer-dump-file-publisher
drc exec delta-producer-background-jobs-initiator curl -X DELETE http://localhost/mandatarissen/healing-jobs
drc exec delta-producer-background-jobs-initiator curl -X POST http://localhost/mandatarissen/healing-jobs # wait until success of the TASK (not the job)
drc exec delta-producer-background-jobs-initiator curl -X DELETE http://localhost/mandatarissen/healing-jobs
drc exec publication-triplestore bash
isql-v;
exec('checkpoint');
exit;
exit;
drc exec delta-producer-background-jobs-initiator curl -X DELETE http://localhost/mandatarissen/dump-publication-graph-jobs
drc exec delta-producer-background-jobs-initiator curl -X POST http://localhost/mandatarissen/dump-publication-graph-jobs # wait until success of the TASK (not the job)
drc exec delta-producer-background-jobs-initiator curl -X DELETE http://localhost/mandatarissen/dump-publication-graph-jobs
git checkout config/delta/rules.js
drc restart deltanotifier
```
After that, ensure `docker-compose.override.yml`
```
  lmb-public-ldes-client:
    environment:
      LDES_BASE: "https://mandatenbeheer.lblod.info/streams/ldes/public/" # Adapt endpoint in function of environment.
      FIRST_PAGE: "https://mandatenbeheer.lblod.info/streams/ldes/public/1"
      BYPASS_MU_AUTH: "false"
```

And now go to `drc up -d`
#### LMB private
In `docker-compose.override.yml`
```
  lmb-private-ldes-client:
    environment:
      LDES_BASE: "https://dev.mandatenbeheer.lblod.info/streams/ldes/abb/" # Adapt endpoint in function of environment.
      FIRST_PAGE: "https://dev.mandatenbeheer.lblod.info/streams/ldes/abb/1"
      BATCH_SIZE: "500"
      EXTRA_HEADERS: '{"Authorization": "Basic encodedString}'
```
```
drc up -d lmb-private-ldes-client
```
#### frontend
Ensure the environment variables are correctly set. See https://github.com/lblod/frontend-loket/pull/408
## 1.104.4 (2024-10-16)
### General
 - Bump export-submissions (DL-6233)
 - Preprocess dates in toezicht export configuration to lessen load on the database (DL-6241)
 - Fix bug in the toezicht export configuration for `submissionDocument` types
### Deploy Notes
#### Manual commands
 - Set `EXPORT_CRON_PATTERN` manually to trigger the export for the first time and observe the logs.
 - If export is successful, update `EXPORT_CRON_PATTERN` to a reasonable value so that exports are triggered automatically starting from the next day.
#### Docker Commands
 - `drc up -d export-submissions`
## 1.104.3 (2024-10-15)
### General
 - Bump export-submissions (DL-6233)
 - Transform toezicht export configuration (DL-6241)
### Deploy Notes
#### Manual commands
 - Set `EXPORT_CRON_PATTERN` manually to trigger the export for the first time and observe the logs.
 - If export is successful, update `EXPORT_CRON_PATTERN` to a reasonable value so that exports are triggered automatically starting from the next day.
#### Docker Commands
 - `drc up -d export-submissions`
## 1.104.2 (2024-09-20)
### General
 - Fix submissions not flagged for export (DL-6182)
### Deploy Notes
#### Docker Commands
 - `drc up -d prepare-submissions-for-export`
#### Manual commands
 - see script Readme in https://github.com/lblod/app-digitaal-loket/pull/599
## 1.104.1 (2024-09-10)
### General
 - Fix failed emails report. (DL-6091)
 - Add cleanup job to routinely move failed emails from the failbox to the outbox. (DL-6091)
### Deploy Notes
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart report-generation resource cache`
 - `drc exec dbcleanup curl -X POST "http://localhost/disableCronJobs" && drc logs -ft --tail=200 dbcleanup`
 - `drc exec dbcleanup curl -X POST "http://localhost/cleanup" && drc logs -ft --tail=200 dbcleanup`
## 1.104.0 (2024-09-05)
### General
#### Frontend
 - `v0.97.0` (DL-6046, DL-6088): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0970-2024-09-05
### Deploy Notes
#### Docker Commands
 - Note: see https://github.com/lblod/frontend-loket/pull/408 for the necessary feature flags
## 1.103.3 (2024-09-02)
### General
 - Fix migrations that removes duplicate URI for IBEG. (DL-5770)
### Deploy Notes
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache`
## 1.103.2 (2024-09-02)
### General
 - Remove duplicate URI for IBEG. (DL-5770)
### Deploy Notes
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache`

## 1.103.1 (2024-08-27)
  - Fix consumer mapping issue [DL-6152]
## 1.103.0 (2024-08-23)
 - updated consumer [DL-5911]
 - update producer [OP-3372]
### deploy notes
#### For the new consumer into account
- Note: the application will be down for a while.
- Ensure application goes down: `drc down`
- Ensure in `docker-compose.override.yml` (on prod)
  ```
  loket:
    image: lblod/frontend-maintenance:0.1.0
    # (...)
   update-bestuurseenheid-mock-login:
     entrypoint: ["echo", "Service disabled to ensure re-sync OP works propery"]
   op-public-consumer:
    environment:
      DCR_SYNC_BASE_URL: "https://organisaties.abb.vlaanderen.be"
      DCR_DISABLE_INITIAL_SYNC: "false"
      DCR_DISABLE_DELTA_INGEST: "false"
      DCR_LANDING_ZONE_DATABASE: "virtuoso" # for the initial sync, we go directly to virtuoso
      DCR_REMAPPING_DATABASE: "virtuoso" # for the initial sync, we go directly to virtuoso
  ```
- `drc up -d migrations loket`
  - That might take a while.
- `drc up -d --remove-orphans `
- Wait until the consumer is finished.
- Enable the frontend, submissions-consumer and update-bestuurseenheid-mock-login
- Ensure op-public-consumer in `docker-compose.override.yml` is syncing with database again
- So the final `docker-compose.override.yml` will look like
  ```
  loket:
    # image: lblod/frontend-maintenance:0.1.0 #comment out the maintenance page
    # (...)
   op-public-consumer:
    environment:
      DCR_SYNC_BASE_URL: "https://organisaties.abb.vlaanderen.be"
      DCR_DISABLE_INITIAL_SYNC: "false"
      DCR_DISABLE_DELTA_INGEST: "false"
  ```
#### remaining bits of deploy
  - ``drc up -d`
## 1.102.1 (2024-08-22)
### General
 - Adjust download/berichtencentrum url warning cleanup job. (DL-6140)
 - Add cleanup job to remove authentication data. (DL-6077)
### Deploy Notes
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache`
 - `drc restart dbcleanup && drc logs -ft --tail=200 dbcleanup`
## 1.102.0 (2024-08-19)
### General
 - Bump `db-cleanup-service` to `v0.5.0`. (DL-5601)
 - Add old error/job cleanup jobs. (DL-5612) (DL-5613)
 - Fix an issue with the job deletion flow in `frontend-dashboard`. (DL-5905)
### Deploy Notes
#### Docker Commands
 - `drc down`
 - `drc up -d virtuoso && drc logs -ft --tail=200 virtuoso`
   - Make sure `virtuoso` is online.
 - `drc up -d migrations && drc logs -ft --tail=200 migrations`
   - Inspect `migrations` logs for any abnormal exit codes, which may indicate an unexpected timeout due to the heavy queries.
 - `drc up -d && drc logs -ft --tail=200 dbcleanup`
   - At service startup, cleanup jobs will be automatically scheduled. You should see an output similar to this: `Job with ID x and title y has been scheduled.`

## 1.101.1 (2024-08-07)
### General
 - Add missing bestuurseenheden [DL-5722]
### Deploy Notes
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache`
## 1.101.0 (2024-08-05)
### General
 - Add open proces huis session role for all organizations [DL-5816]
 - Bumped delta-producer-publication-graph-maintainer.
 - Fixed failed emails report. [DL-6044]
 - Link Toezichthoudende Provincie Antwerpen to "Orthodoxe Parochie Heilige Sophrony de Athoniet" [DL-6014]
#### Frontend
 - `v0.96.0` (DL-4540, DL-4069, DL-6053): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0960-2024-08-05
 - `v0.95.0` (DL-6042, DL-6050): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0950-2024-07-11
 - `v0.94.1` (DGS-316): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0941-2024-06-25
 - `v0.94.0` (DL-5816, DGS-161): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0940-2024-06-19
### Deploy Notes
On production, remove the delta-producer-publication-graph-maintainer image in the docker-compose.override.yml.
#### Docker Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart report-generation resource cache`
 - `drc up -d loket`
## 1.100.2 (2024-07-16)
### General
  - [DL-6049] Add missing organizations that are present in OP and Kalliope but not in Loket.
### Deploy Notes
  - `drc restart migrations && drc logs -ft --tail=200 migrations`
  - `drc restart resource cache`

Make sure to change `MAX_MESSAGE_AGE` from `2` to `30` for `berichtencentrum-sync-with-kalliope` in `docker-compose.override.yml`:
  - `drc up -d berichtencentrum-sync-with-kalliope`

Once the logs have indicated a successful resync, restore the value of `MAX_MESSAGE_AGE` from `30` back to `2`:
  - `drc up -d berichtencentrum-sync-with-kalliope`
## 1.100.1 (2024-07-03)
### Berichtencentrum
  - [DL-6020] Fix an issue where the configured email would revert to the old value after updating it
### Deploy Notes
  - `drc restart migrations resource cache`
## 1.100.0 (2024-06-14)
### Fixes
- Bump delta-producer-publication-graph-maintainer [DL-4527] and related [OP-3151]
- Bump `vendor-data-distribution` for healing, needed for [DL-5925]. Already been deployed via overrides in `docker-compose.override.yml`. Please remove the image override there.
### Toezicht
- DL-5856: ensure some type of submissions are not exported to `app-toezicht-abb`
  - See also: DL-5922
### Subsidy
- DGS-298: update lokaal bestuurlijk talent subsidy enddate
### Deploy Notes
- `drc up -d delta-producer-publication-graph-maintainer export-submissions; drc restart deltanotifier migrations cache resource; drc up -d`
- Remove the image override for service `vendor-data-distribution` in `docker-compose.override.yml`
## 1.99.1 (2024-05-31)
### General
  - Hotfix: update lokaal bestuurlijk talen deadline
## 1.99.0 (2024-05-31)
### Reports
  - Add new report `recentEmailsInFailbox` which tracks failed emails (DL-5943)
### Deploy Notes
  - `drc restart migrations report-generation`
## 1.98.2 (2024-05-29)
  - Fix custom info label field in forms LEKP-rapport - Melding correctie authentieke bron and LEKP-rapport - Toelichting Lokaal Bestuur (DL-5934)
### Deploy Notes
  - `drc up -d enrich-submission; drc restart migrations resource cache`
## 1.98.1 (2024-05-27)
### Fixes
 - Bump `worship-positions-graph-dispatcher-service-loket` to fix missing data in some organisations (DL-5823). This new version is better at dispatching data with its entire hierarchical model. For this, a migration needs to run to completion and this service then needs to be restarted. You can `drc up -d` it at the end of the deploy. This is included in the commands below.
### Commands
 - `drc restart migrations && drc logs -ft --tail=200 migrations` (wait for these to complete)
 - `drc up -d dispatcher-worship-mandates`
## 1.98.0 (2024-05-16)
#### Frontend
 - `v0.93.1` (DL-5888): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0931-2024-05-06
 - `v0.93.0` (DL-5849): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0930-2024-05-02
### General
 - Consolidation worship-sensitive delta-producer (DL-5588)
 - Bump automatic submission service (no jira reference)
   - See: https://github.com/lblod/automatic-submission-service/commit/7e938c07cd9434986dbd0010843b704a5ae7302f
 - Adjust reports `Toezicht module: Meldingen`, `Eredienst mandatarissen` and `Message report with provenance` (DL-5836) (DL-5815)
 - User impersonation for admin users (DL-5757)
 - Update forms
  - Adjust LEKP rapport Klimaattafels (DL-5832)
  - Add new LEKP rapport Wijkverbeteringscontract (DL-5829)
#### Fixes
 - Fix reports with too many quotes around fields in the data. (DL-5811)
 - Bump `deltanotifier` (DL-5684)
### Deploy Notes
#### Consolidation Worship-Sensitive Delta-Producer
##### Edit `config/delta-producer/background-jobs-initiator/config.override.json`
 - Copy `worship-services-sensitive` config from `config/delta-producer/background-jobs-initiator/config.json` to the override file.
 - Change `"startInitialSync"` from `false` to `true`.
##### Edit `config/delta-producer/publication-graph-maintainer/config.override.json`
 - Copy `worship-services-sensitive` config from `config/delta-producer/publication-graph-maintainer/config.json` to the override file.
 - Add `"key": "<producer_key>"` at the end of each stream's config; check `docker-compose.override.yml` for the value of that key.
##### Don't Forget!
The API broke in the dispatcher, which makes sense because there was an error. But of course, we have to be careful; the consumers might depend on it.
Luckily, it's very likely we can access the consumers, so we'll have to go on tour and update the paths where they connect to.
###### app-worship-organizations, app-organization-portal
On PROD, QA, and DEV, in `docker-compose.override.yml` change
  `DCR_SYNC_LOGIN_ENDPOINT: 'https://loket.lokaalbestuur.vlaanderen.be/sync/worship-services-sensitive-deltas/login'`
  to
  `DCR_SYNC_LOGIN_ENDPOINT: 'https://loket.lokaalbestuur.vlaanderen.be/sync/worship-services-sensitive/login'`.
##### Frontend
 - Remove the image override. v0.91.3 is currently deployed but the same fix is part of v0.93.1 so we can safely remove it.
##### Controle environment
We no longer need this environment since we now have the admin role and impersonation feature. We can remove the controle environments from both QA and PROD.
- Remove the "controle" service from `docker-compose.override.yml`
- The DNS configuration for the following domains is no longer needed so ask Aad to remove it once the change is deployed to production:
  - **controle.lblod.info**
  - **qa.controle.lblod.info**

> The PROD environment has more controle-* prefixed services, but we can't remove those yet since they are used by the `vendor-management` and `dashboard` services as well.

The standard `docker-compose.yml` config seems in accordance with what is provided by the dispatcher. Remember Loket exposed two flavors of paths, one for the files and one for the login.
#### Docker Commands
 - `drc up -d --remove-orphans deltanotifier automatic-submission loket enrich-submission`
 - `drc restart report-generation`
 - `drc restart delta-producer-publication-graph-maintainer`
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache database dispatcher identifier`
## 1.97.2 (2024-05-02)
### General
 - bump-berichtencentrum (DL-5775)
### Deploy notes
`drc up -d berichtencentrum-email-notification`
## 1.97.1 (2024-04-30)
### Subsidies
 - Update Lekp 1.0 (2021 - 2024) opvolgmoment 2024 deadline (DGS-238)
### General
#### Fixes
  - Update predicates to export for `melding:FormData` (in context of DL-5738)
### Deploy notes
 - `drc restart delta-producer-publication-graph-maintainer`
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart resource cache`
## 1.97.0 (2024-04-12)
### General
#### Fixes
 - Bump `berichtencentrum-email-notification` service, which fixes the general problem where no emails about messages get sent. (DL-5775)
    - > Note: It will already be deployed on production (`docker-compose.override.yml`) before this release gets deployed.
 - Bump `adressenregister-fuzzy-search-service` to `v0.8.0` (DL-5822)
 - Add logging config for the `dispatcher-worship-mandates` service (DL-5818)
#### New Organizations
 - Add a new politiezone: `PZ Aalter/Maldegem: Aalter en Maldegem` (DL-5730)
 - Add a new OCMW vereniging: `Ter Lembeek` (DL-5739)
### Subsidies
 - Add new 'Lokaal Bestuurlijk Talent' subsidy (DGS-184)
### Deploy Notes
  - Remove the pinned image of `lblod/berichtencentrum-email-notification-service:0.4.1` in `docker-compose.override.yml`
#### Docker Commands
 - `drc up -d adressenregister dispatcher-worship-mandates loket`
 - `drc restart migrations && drc logs -ft --tail=200 migrations`
 - `drc restart subsidy-applications-management`
 - `drc restart resource cache`
## 1.96.0 (2024-03-25)
### Reports
 - Changed Report on Berichten: increased history to 12 months, changed column order and attachments are formatted with their filename. (DL-5696)
### General
#### Backend
 - Bump `berichtencentrum-sync-with-kalliope` to `v0.20.0` (DL-5629, DL-5748)
 - Bump `berichtencentrum-email-notification-service` to `v0.4.0` (DL-5629, DL-5748)
 - Bump `loket-report-generation-service` to `v0.8.2` (DL-5696)
 - Add deltas support for vendor management data (DL-5667)
   - Adds `vendor-management` as a new delta-stream
 - Add a new "Virus scan" report (DL-5618)
### Deploy Notes
 - Remove **berichtencentrum-sync-with-kalliope** `v0.20.0-rc.1` image from `docker-compose.override.yml` on QA
 - Restart the `report-generation` service to pick up the new report config
#### Delta Producer Config Changes for `publication-graph-maintainer` and `background-jobs-initiator`
##### Edit `config/delta-producer/background-jobs-initiator/config.json`
 - Change `"startInitialSync"` from `false` to `true` for `vendor-management`.
 - Change `"disableDumpFileCreation"` from `true` to `false` for `vendor-management`.
##### Edit `config/delta-producer/publication-graph-maintainer/config.json`
 - Add `"key": "<producer_key>"` for the new `vendor-management` delta stream.
#### Docker Commands
 - `drc up -d virus-scanner berichtencentrum-sync-with-kalliope berichtencentrum-email-notification report-generation`
 - `drc restart migrations dispatcher deltanotifier delta-producer-background-jobs-initiator delta-producer-publication-graph-maintainer delta-producer-dump-file-publisher jobs-controller report-generation`
## 1.95.0 (2024-03-21)
### Toezicht
 - Adjust/Add new forms and new codelists (DL-5669 - DL-5625 - DL-5643 - DL-5646 - DL-5665 - DL-5670) : See full changes on https://github.com/lblod/app-digitaal-loket/pull/528
### Subsidies
 - Add Fietssubsidie types via extractor (DGS-94)
 - Add LEKP 1.0 (2021) types via extractor (DGS-107)
 - Add slaapplekken subsidy types via extractor (DGS-165)
 - Add Nooddorpen subsidy types via extractor (DGS-166)
 - Update LEKP 1.0 (2021) deadlines (DGS-167)
 - Update LEKP 2.0 deadlines (DGS-168)
 - Update LEKP 2.1 deadlines (DGS-169)
### General
#### Frontend
 - `v0.91.1` (DL-5751): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0911-2024-03-13
 - `v0.91.0` (DL-5735): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0910-2024-03-12
#### Backend
 - Bump `migrations` to `v0.9.0`
 - Bump `mocklogin` to `v0.4.0` (DL-5709)
 - Bump `vendor-data-distribution-service` to `v1.3.3` (DL-5683)
 - Bump `enrich-submission` to `v1.11.0` (DL-5646 & DL-5670)
 - Bump `prepare-submissions-for-export` to `v0.9.0` (DL-5643 - DL-5646 - DL-5670)
### Deploy Notes
 - Remove the frontend `v0.90.3` image override from `docker-compose.override.yml`
 - update the controle image to `v0.91.1-controle` in the `docker-compose.override.yml` file
 - Bump the `mocklogin` image in `docker-compose.override.yml` to `lblod/mock-login-service:0.4.0`
#### Docker Commands
 - `drc up -d loket controle migrations mocklogin vendor-data-distribution enrich-submission prepare-submissions-for-export`
 - `drc restart migrations`
 - `drc restart subsidy-applications-management delta-producer-publication-graph-maintainer-subsidies resource cache`
## 1.94.0 (2024-02-19)
### Subsidies
 - Add new stadsvernieuwing - conceptsubsidie || Oproep 2024 reeks (DGS-154)
### General
#### Frontend
 - Bump frontend to `v0.90.2` (DL-5537, DL-5686): https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0902-2024-02-19
#### Backend
 - Consolidate `mandatarissen` delta flow (DL-5585, DL-5586)
 - Consolidate `leidinggevenden` delta flow (DL-5582, DL-5583)
 - Move `delta-producer-dump-file-publisher` publication graph endpoint from `virtuoso` to `publication-triplestore` (DL-5584)
 - Bump `delta-producer-publication-graph-maintainer` (DL-5584)
 - Added virus-scanner service (DL-5553)
 - Bump `dispatcher-worship-mandates` service to v1.4.0 to use vendor provance for dispatching harvested data. This hopefully resolves some of the issues where organisations could see data that they did not publish themselves. (DL-5567, DL-5568, DL-5569)
 - Add migration to remove all previously harvested data. Harvesting needs to be completely restarted.
### Toezicht
 - Add a future date warning validation to the "Datum zitting" fields (DL-5624)
### Deploy Notes
#### Config Delta Mandatarissen & Leidinggevenden
##### Edit `config/delta-producer/background-jobs-initiator/config.json`
 - Change `"startInitialSync"` from `false` to `true`.
##### Edit `config/delta-producer/publication-graph-maintainer/config.json`
 - Add `"key": "<producer_key>"` at the end of each stream's config; check `docker-compose.override.yml` for the value of that key.
##### Edit `docker-compose.override.yml`
 - Remove the specific entries from `docker-compose.override.yml` for:
   - `delta-producer-background-jobs-initiator-mandatarissen`
   - `delta-producer-background-jobs-initiator-leidinggevenden`
   - `delta-producer-publication-graph-maintainer-mandatarissen`
   - `delta-producer-publication-graph-maintainer-leidinggevenden`
#### Move `dump-file-publisher` data to `publication-triplestore`
 - Follow the steps in [this guide](https://github.com/Riadabd/dump-and-import-publication-graphs)
#### Worship services consumer and dispatcher
 - The harvesting for the worship services needs to be completely restarted. It could be helpful to disable the `app-lblod-harvester-worship` stack (or its `identifier` service) while performing these steps, so that new jobs are not started while trying to remove data from old jobs.
   - Make sure the latest migrations are finished, more specifically: `20240131174900-flush-harvested-data.sparql`
   - Make sure the graphs `http://eredienst-mandatarissen-consumer/temp-inserts`, `http://eredienst-mandatarissen-consumer/temp-deletes` and `http://eredienst-mandatarissen-consumer/temp-discards` are empty. If not, please execute a simple query to do so.
   - Flush sync jobs from the consumer with its API:
```
 drc exec eredienst-mandatarissen-consumer bash
 # curl -X POST http://localhost/flush
   {...warning output, continue...}
```
   - OPTIONAL: You can remove (all) historical consumer files from disc on the path `data/files/consumer-files/eredienst-mandatarissen/`. Files are structured in folders based on the date.
   - Inspect the logs of the `eredienst-mandatarissen-consumer` to see that the flush job was successful and that the periodic consumer job is running (every minute(?)).
#### Controle
 - Update the version of the controle image in the docker-compose.override.yml file
#### Docker commands
 - `drc up -d --remove-orphans`
 - `drc restart migrations dispatcher deltanotifier delta-producer-background-jobs-initiator delta-producer-publication-graph-maintainer delta-producer-dump-file-publisher`
 - `drc restart resource cache`
## 1.93.1 (2024-02-16)
### General
#### Frontend
 - Bump frontend to `v0.89.2` (DL-5537)
   - Fixes issue with `contact` and `verenigingen` module cards not showing when logging through ACM.
#### Backend
 - Fix `contact` and `verenigingen` session roles for `mock-login` users (which includes controle) (DL-5537).
### Deploy Notes
#### Controle
 - Update the version of the controle image in `docker-compose.override.yml`
#### Docker Commands
 - `drc up -d loket controle`
 - `drc restart migrations`
 - `drc restart resource cache`
## 1.93.0 (2024-01-23)
### General
### Backend
 - Add mock-login session roles for `contact` and `verenigingen` apps (DL-5599)
 - Bump `berichtencentrum-sync-with-kalliope` to `v0.18.0` (DL-5560)
 - Replace wrong kbo-number with the correct one for s-Lim (DL-5609)
### Frontend
 - Bump frontend to `v0.89.0`
   - Add `contact` and `verenigingen` module cards with external links (DL-5537, DL-5538)
   - Move subsidy warning to bottom of the page (DGS-111)
### Deploy Notes
#### Frontend
 - Add the relevant external URLs (QA or PROD) to `docker-compose.override.yml` under the `environment` section of `loket`; the URLs can be found in DL-5537 and DL-5538:
   - `EMBER_CONTACT_URL: "{{URL}}"`
   - `EMBER_VERENIGINGEN_URL: "{{URL}}"`
#### Docker Commands
 - `drc up -d loket berichtencentrum-sync-with-kalliope`
 - `drc restart migrations`
 - `drc restart resource cache`
## 1.92.0 (2024-01-17)
### General
#### Backend
 - bump resource to `semtech/mu-cl-resources:feature-differently-stable-luckless`
   - Fixes: Error 500 in some rare cases when fetching submissions
 - bump `berichtencentrum-sync-with-kalliope` to fix last message regression
 - Remove `delta-producer-background-jobs-initiator-worship-submissions` from `docker-compose.yml`
 - Remove `delta-producer-publication-graph-maintainer-worship-submissions` from `docker-compose.yml`
 - Consolidate the above into `delta-producer-background-jobs-initiator` and `delta-producer-publication-graph-maintainer`
#### Frontend
 - Bump frontend to `v0.88.3`
  - Fixes: Text clipping on `Nationaliteit` placeholder in `Mandatenbeheer` for worship services
  - Fixes: preview links in the "Toezicht" module
### toezicht
- Update forms
    - New forms LEKP Collectieve Energiebesparende Renovatie, Fietspaden, Sloopbeleidsplan
    - New forms Niet-bindend advies op statuten and Niet-bindend advies op oprichting
    - Change form LEKP Melding correctie authentieke bron, removed field "type correctie"
    - Bump enrich-submission v1.8.0
### subsidies
- Extend deadline nooddorpen
- Extend deadline oekraine slaapplekken
- Update lekp 2.1 and 2.0 opvolgmoment titles

### Deploy notes
#### Config: docker-compose.override.yml
 - Remove line `image: semtech/mu-cl-resources:feature-differently-stable-luckless` from `docker-compose.override.yml` on production.
 - Remove line `image: lblod/berichtencentrum-sync-with-kalliope-service:0.17.2-rc.1` from `docker-compose.override.yml` on production.
 - We improved on the queries a lot, they should be light enough to work
   through mu-auth, but if not, add the following config in
   `docker-compose.override.yml` to connect to Virtuoso directly:
  ```
  vendor-data-distribution:
    environment:
      SPARQL_ENDPOINT_COPY_OPERATIONS: "http://virtuoso:8890/sparql"
  ```
#### Config Delta Worship Submissions
##### Edit `config/delta-producer/background-jobs-initiator/config.json`
 - Change `"startInitialSync"` from `false` to `true`.
##### Edit `config/delta-producer/publication-graph-maintainer/config.json`
 - Add `"key": "<producer_key>"` at the end of each stream's config; check `docker-compose.override.yml` for the value of that key.
##### Edit `docker-compose.override.yml`
 - Remove the specific entries for `delta-producer-background-jobs-initiator-worship-submissions` and `delta-producer-publication-graph-maintainer-worship-submissions` from `docker-compose.override.yml`.
#### docker commando
 - `drc up -d --remove-orphans; drc restart migrations resource cache dispatcher deltanotifier delta-producer-background-jobs-initiator delta-producer-publication-graph-maintainer`
## 1.91.1 (2023-12-11)
 - Fix s-limburg and a2gb start dates
## 1.91.0 (2023-12-09)
### Subsidies
 - update active step for LEKP 1.0 to new opvolgmoment 2024 step for all consumptions
## 1.90.0 (2023-12-08)
### General
 - add LEKP 1.0 - 2022 || opvolgmoment 2024 step
 - fix berichtencentrum-sync-with-kalliope service to work better VDDS
## 1.89.1 (2023-12-07)
### General
  - hotfix: improve migration speed
## 1.89.0 (2023-12-06)
### General
 - Delta-dump-file producer tweaks for mandatarissen/leidinggevenden
 - Maintenance of persons-sensitive deltas
 - Reports: vendors/consumers and Loket.
 - Added mockuser: Zorgband Leie en Schelde
### Submissions
 - Added RO submissions for export: [471](https://github.com/lblod/app-digitaal-loket/pull/471)
 - Adjust form 'Goedkeuringsbesluit budget(wijziging)' (Gemeente and Provincie)
### Subsidies
 - Producer config update
### LPDC
- Remove all LPDC code, data, reports and old migration files
### Berichtencentrum
- Added berichten-centrum integration, which entails:
  - Added `berichten-melding-service`. This service allows vendors to reports
    messages, and imports them into the Berichtencentrum module.
  - Added config for `vendor-data-distribution-service`: in its newer version,
    the config now uses type, trigger, path, remove and copy properties to select
    and copy data to the vendor graphs. These changes where needed to integrate
    with the `berichten-melding-service`.
  - Added a new job for the `job-controller-service` for the Berichten Melding.
  - Added dispatcher path for the `berichten-melding-service`.
  - Added error emails for `berichten-melding-service` and
    `vendor-data-distribution-service`.
  - Added `delta-notifier`, `mu-authorization` config.
  - A Message now also has a `creator`. This is used to trigger the
    `vendor-data-distribution-serivce`.
  - Added a reports on Messages and Conversations of the past 6 months. This is
    almost a dump of the entities and can be used to compare between
  - Added migrations to copy Messages and Conversations to Vendor Graphs for
    Vendor API such that vendors can access that data through their authorised
    SPARQL client. **NOTE: these migrations can take a long time to run!**
  - New Loket frontend: adding creator to messages and other various improvements
    to conversations and messages in Berichtencentrum.
### Deploy Instructions
#### LPDC
A lot of data has to be removed, so it has to be backed up first and docker
images have to be cleaned:
- Back up the database before any service(s) is(are) restarted
- Remove cronjobs for LPDC related services from the server
- `drc restart migrations` to run the new migrations and remove LPDC data
- `drc up --remove-orphans` to remove orphaned docker images
#### Persons-sensitive-consumer
For persons-sensitive maintenance **don't forget to add `"key": "<producer_key>"` in `config/delta-producer/publication-graph-maintainer/config.json `of persons-sensitive** then `drc up -d --remove-orphans`
#### Berichtencentrum
Run the following query on the stack with sufficient privileges **after all
migrations have run**. This does not have to go through mu-auth. **Make sure to
set the real hostname of the running stack in place of `<lokethostname>`
below.**

```sparql
PREFIX sch:     <http://schema.org/>
PREFIX nie:     <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
PREFIX nfo:     <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX account: <http://mu.semte.ch/vocabularies/account/>
PREFIX core:    <http://mu.semte.ch/vocabularies/core/>

INSERT {
  GRAPH ?vendorGraph {
    ?attachment nie:url ?attachmentDownloadLink .
  }
}
WHERE {
  GRAPH ?vendorGraph {
    ?message
      a sch:Message ;
      nie:hasPart ?attachment .
    ?attachment
      a nfo:FileDataObject ;
      core:uuid ?attachmentUUID .

    BIND (CONCAT("https://<lokethostname>.be/files/", ?attachmentUUID, "/download") AS ?attachmentDownloadLink)
  }
  FILTER(regex(STR(?vendorGraph), "http://mu.semte.ch/graphs/vendors/*", "i"))
}
```

## 1.88.1 (2023-11-22)
### Subsidies
- extend ukraine deadlines and update oproep
### Deploy Instructions
```
drc restart migrations
drc restart resource cache
```

## 1.88.0 (2023-10-31)
### general
- update virtuoso
### Deploy instructions
- see: https://github.com/lblod/app-digitaal-loket/pull/426
## 1.87.0 (2023-10-20
)
### general
- bump cache graph maintainer
- final version berichtencentrum-sync-with-kalliope
### inzendingen
- update time window of export: start from 01-09-2023 now
### subsidies
- make attachments availible over deltas
- fix login endpoint for delta-consumer
### Deploy instructions
Note: the deploy instructions assume we come from 1.85.0. So you might restart a bit too much. But that's okay.
```
drc restart virtuoso publication-triplestore
```
```
drc restart export-submissions berichtencentrum-sync-with-kalliope delta-producer-publication-graph-maintainer-subsidies delta-producer-publication-graph-maintainer-worship-submissions dispatcher migrations report-generation resource enrich-submission validate-submission cache; drc up -d
```
## 1.86.0 (2023-10-17)
- update forms
- add status to worship mandates producer
### Deploy notes
- drc restart migrations berichtencentrum-sync-with-kalliope delta-producer-publication-graph-maintainer-ws-sensitive ; drc up -d prepare-submissions-for-export-service
## 1.85.3 (2023-10-19)
### General
 - Bump multiple delta-related services
 - Tweak virtuoso and publication datastore to account for bigger datasets
### Submissions
 - prepare migration to export besluitenlijsten politiezones
 - bump prepare-submission-for-export to have live streaming politiezones
### Deploy instructions
```
drc restart virtuoso publication-triplestore migrations; drc up -d
```
## 1.85.2 (2023-10-16)
### Erediensten
- fix reports: extra fixes, plus added extra info for bedienaren.
## 1.85.1 (2023-10-13)
### General
- bump report-generation
### Erediensten
- fix reports: improved code and removed bugs
## 1.85.0 (2023-09-22)
### General
- Add an environment variable to the frontend that can be used to display a "global system notification" to the users
- Bump identifier
### Erediensten
- Bumped version berichtencentrum with kalliope, to have less data coming through
- Bump report generation; better character escape
- Improved mandatarissen-report

### Deploy instructions
Automatic submission as very hot fix in docker-compose.override.yml. Remove this.
If a maintenance message needs to be displayed, uncomment the environment variable in the docker-compose.override.yml file and edit the message.
```
drc up -d loket controle automatic-submission berichtencentrum-sync-with-kalliope report-generation identifier
```
## 1.84.3 (2023-09-22)
- added ag2b
## 1.84.2 (2023-09-21)
- besluitenlijst politiezone
- new producers, because much data
-
### Deploy instructions
In your docker-compose.override.yml
- Ensure delta-producer-background-jobs-initiator-submissions is renamed to delta-producer-background-jobs-initiator
- Ensure delta-producer-publication-graph-maintainer-submissions is renamed to delta-producer-publication-graph-maintainer

In file config/delta-producer/background-jobs-initiator/config.json ensure `"startInitialSync": false` is set to `"startInitialSync": true,`
In file config/delta-producer/publication-graph-maintainer/config.json ensure
```
# ...
"key": "the key in the docker-compose override previously"
```
Then
```
drc up -d --remove-orphans; drc -r restart dispatcher deltanotifier migrations
```

## 1.84.1 (2023-09-01)
  - Startdate lekp 1 & 2 set to august 31 (past)
## 1.84.0 (2023-08-31)
### general
- various improvements automatic-submissoin
  - bugfixes, cleanup
  - feature: delete submission
### subsidies
- VGC added for Stadsvernieuwing - thematische subsidie
### Deploy instructions
```
drc up -d; drc restart migrations resource cache subsidy-applications-management database dispatcher
```
## 1.83.4 (2023-08-29)
### LPDC
  - bump ldes-consumer
### Deploy instructions
```
drc up -d lpdc-ldes-consumer
```
## 1.83.3 (2023-08-25)
### General
  - Add autonoom gemeentebedrijf mercator APB Kappelen
### Deploy instructions
```
drc up -d migrations
```
and trigger the `update-bestuurseenheid-mock-login` service.
## 1.83.2 (2023-08-23)
### LPDC
  - cleanup `&nbsp` from `dct:description`
## 1.83.1 (2023-08-08)
  - add uuids to subsidy producer export config
## 1.83.0 (2023-07-25)
### General
  - added AGB berlaar bestuurseenheid
### Worship services
  - setup worship consumer to harvest from vendor
  - added reports to report on harvested information
### personeelsbeheer
  - added IFIC personeel
###  subsidies
  - moved deadlines LEKP subsidies
### Deploy instructions
```
drc restart migrations resource cache deltanotifier; drc up -d
```
## 1.82.5 (2023-07-19)
### LPDC
  - bump frontend (fixing issue archived vs revision in the UI)
### Deploy instructions
  - `drc up -d loket` to fetch the new version
## 1.82.4 (2023-07-19)
### LPDC
  - bump `lpdc-management-service` to v0.24.4
### Deploy instructions
  - `drc up -d lpdc-management` to fetch the new version
## 1.82.3 (2023-07-19)
### LPDC
  - bump `lpdc-management-service` to v0.24.3
### Deploy instructions
  - `drc up -d lpdc-management` to fetch the new version
## 1.82.2 (2023-07-19)
### Leidinggevenden
  - Update config so correct predicate for postal code is exported
### Deploy instructions
```
drc restart delta-producer-publication-graph-maintainer-leidinggevenden
```
## 1.82.1 (2023-07-18)
### LPDC
  - bump `lpdc-management-service` to v0.24.2
### Deploy instructions
  - `drc up -d lpdc-management` to fetch the new version
## 1.82.0 (2023-06-30)
### General
  - Frontend [v0.80.0 - v0.84.0](https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0840-2023-06-22)
  - bump `berichtencentrum-email-notification-service`
  - bump `mu-resource and mu-auth`
### Inzending voor toezicht
  - Only toezichthoudende betrokkenheid should be shown (hence bump enrich-submission)
  - Update besluit handhaven form label to Besluit handhaven na ontvangst schorsingsbesluit
  - Update semantic form to include a new custom alert component in the Besluit handhaven na ontvangst schorsingsbesluit form
### Erediensten
  - flush, rerun op sync migrations to fix typeBetrokkenheid
### Subsidies
  - Add two new external subsidies (Hoppinpunten and Haltes)
### LPDC
  - Add "nieuw" and "toegevoegd" labels and filters to concepts
  - Add support for concept archiving
### deploy instructions
#### Virtuoso update
  - stop the stack: `drc stop`
  - start the "maintenance" frontend?
  - start the virtuoso container: `drc start virtuoso`
  - create a new checkpoint so the transaction log is cleared:
      - enter the isql-v environment: `drc exec virtuoso isql-v`
      - create a checkpoint: `exec('checkpoint');`
      - exit the isql-v environment: `exit;`
  - stop the virtuoso service
  - make a backup of the data/db folder in case something goes wrong and we need to downgrade again `cp -r data/db some-backup-folder`
  - remove the transaction log file: `rm data/db/virtuoso.trx`
  - `drc up -d virtuoso` to update to the new image
  - check the logs to see if nothing strange happened
#### Other
- Follow the steps to re-sync data from OP
- `drc restart migrations` and check if they ran successfully
- `drc restart cache resource database dispatcher subsidy-applications-management report-generation enrich-submission; drc up -d`
## 1.81.1 (2023-05-18)
### Inzending voor toezicht
  - bump import submission: centrale besturen should also be able to provide normal attachements.
### deploy instructions
```
drc up -d
```
## 1.81.0 (2023-05-18)
### General
  - Frontend [v0.79.3](https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0793-2023-05-04)
  - Bump adressenregister services
  - Frontend vendor access management: allows setting read-only
### Subsidies
  - Add new thematische stadsvernieuwing subsidie
  - Add LEKP 2.1 Indienen Pact report
### LPDC
  - Remove the English requirement for text fields when publication channel "YourEurope" is not chosen
### deploy instructions
  - update the `-prod` and `-controle` frontend images to the correct version
  -
    ```
    drc restart migrations
    ```
    Check the migrations were ok.
    ```
    drc restart cache resource database subsidy-applications-management report-generation; drc up -d
    ```
### Extra notes
 - The bump of mu-resource and mu-auth (i.e. the performance improvement) has been postponed dueu to new bugs.
## 1.80.3 (2023-05-17)
### Inzending voor toezicht
  - hotfix: vendor-data-distrbution-service breaking data.
  - Historical data for vendor-api is availible now.
### deploy instructions
```
drc up -d; drc restart migrations
```
## 1.80.2 (2023-05-16)
### General
  - add new bestuurseenheid (`Politiezone Rivierenland`)
## 1.80.1 (2023-05-10)
### general
  - Added vendor API
### deploy instructions
```
drc restart database dispatcher deltanotifier; drc up -d
```
## 1.80.0 (2023-04-30)
### general
  - Frontend [v0.78.0 & v0.79.0](https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0790-2023-04-24)
  - data: remove redundant person URIs for rotselaar councillor
  - Frontend-vendor access management (note: this is bundled in a release, it was already 'hot'-deployed)
  - Improve Readme
  - Increased virtuoso memory for default deploys
### Inzending voor toezicht
 - remove Advies samenvoeging for Gemeente
 - remove Verslag lokale betrokkenheid for Gemeente and Provincie
 - A bit of a performance tweak to diminish the load import/export submissins from loket to toezicht abb
 - Fix bug automatic-submission (i.e. jobs not consistently failed weh download failed)
### LPDC
  - make the "uitvoerende overheid" an optional field
  - fix the creation of empty public service instances for some users
  - Add more organization types (Gemeenten, OCMW, Autonoom Provinciebedrijf, Autonoom Gemeentebedrijf, etc.) to the "Uitvoerende overheid" and "Bevoegde Overheid" fields
  - "YourEurope categorien" is now required if "YourEurope" is selected for "Publicatiekanalen"
### Subsidies
  -  [#389](https://github.com/lblod/app-digitaal-loket/pull/389) Change date and title for subsidie opknapwerken slaapplekken oekraine
  - new "Stadsvernieuwing fase 2" subsidy
  - Update deadlines Opknapwerken slaapplekken Oekraïne subsidy
  - Increase max body size `subsidy-applications-management`
### deploy instructions

  - update the `controle` image to `lblod/frontend-loket:0.79.0-controle` (renamed from lblod/frontend-loket:x.x.x-batch-edit)
  - Remove the image from ` image: lblod/frontend-vendor-access-management:0.7.0` from `docker-compose.override.yml`
```
  drc restart migrations cache resource dispatcher subsidy-applications-management enrich-submission export-submissions; drc up -d
```
## 1.79.2 (2023-04-27)
### Subsidy
  - Hotfix: "relevante bijlages" field in the Stadsvernieuwing - Projectsubsidie (Fase 1) and Stadsvernieuwing - Conceptsubsidie subsidies should be optional
### deploy instructions
```
  drc restart subsidy-applications-management
```
## 1.79.1 (2023-04-25)
### Toezicht
  - Added document 'afwijking principes regiovorming'.
### deploy instructions
```
  drc restart migrtions; drc up -d
```
### ⚠️notes
This is a replay from hotfix `1.78.5` which has accidently started and released from the wrong tag.
## 1.79.0 (2023-04-06)
### General
 - Added mock-login creator on new bestuurseenheid
### Worship
 - Consume data from Organisations portal: Erediensten
### deploy instructions
 - Make backup of virutoso
 - Run migrations first
 - `drc up -d`
 - Wait until the stack starts properly
 - Start the intial sync

## 1.78.5 (2023-04-24)
### Toezicht
  - Added document 'afwijking principes regiovorming'.
### deploy instructions
```
  drc restart migrations; drc up -d
```
## 1.78.4 (2023-04-04)
### Toezicht
#### backend
  - hotfix: submission filters have been changed to reduce load on system
## 1.78.3 (2023-03-30)
### General
  - added report links accross organisations
### Inzendingen voor toezicht
  - Remove list of submission to be sent to Kalliope
### deploy instructions
```
drc restart berichtencentrum-sync-with-kalliope report-generation
```
## 1.78.2 (2023-03-29)
### Sync with kalliope
  - hotfix: removed unused uri to reduce warnings
## 1.78.1 (2023-03-24)
### Worship
  - worship submissions: share publication date
     - deploy notes: `drc restart delta-producer-publication-graph-maintainer-worship-submissions`
## 1.78.0 (2023-03-23)
  - Frontend [v0.77.0 & v0.77.1](https://github.com/lblod/frontend-loket/blob/development/CHANGELOG.md#v0771-2023-03-22)
  - Vendor management frontend v0.6.0:
    - Hopefully fix a data issue by changing the way we persist changes
    - UX/UI improvements
    - General maintenance
### LPDC
  - maintain the creation order of listings in the semantic forms
### Migrations
  - updated kbo number for st andries in antwerp
## 1.77.9 (2023-03-15)
### Worship
  - fix relationship provincies and Kathedralen
## 1.77.8 (2023-03-03)
### General
  - migration to avoid obsolete failed urls will trigger a warning

## 1.77.7 (2023-03-03)
### General
  - update report a little 'hanging submissions'
### Inzendingen voor toezicht
  - Download url service robustness
  - Download url warning service
## 1.77.6 (2023-03-02)
### General
  - extra report for non-sent submission
### LPDC
  - bump lpdc-ldes-consumer
## 1.77.5 (2023-02-27)
  - Add LEKP 1.0 Opvolgmoment report
## 1.77.4 (2023-02-24)
  - Update deadline opvolgmoment LEKP 1.0 subsidy
## 1.77.3 (2023-02-16)
### migrations
  - Add a new subsidy-measure: Stadsvernieuwing - conceptsubsidie
### LPDC
  - Add dditional description field to lpdc forms
## 1.77.2 (2023-02-16)
### Worship services
  - Add representatieve organen to Vanden Broele
### General
 - Improved date entry UX by using a date input field instead of a date picker
### LPDC
 - Update to the new v2 editor
## 1.77.1 (2023-02-10)
### Worship services
  - Fix automatic submissions for religiopoint
## 1.77.0 (2023-02-08)
### General
  - Show a message about the self-registration option in ACM/IDM
  - Set up Plausible analytics
### LPDC
  - Implement the basic "review status notification" system
### Worship services
  - read only modules if the data is managed by an external vendor
  - add info messages so users know which positions they need to add
  - Remove Mandaat 'Bestuurslid van Eredienst'
### Inzendingen voor toezicht
  - Fixing issues 'Rechtspositieregeling' APB
### Subsidies
  - Stadsvernieuwing
## 1.76.4 (2023-07-02)
### Berichtencentrum
  - fix config issue
## 1.76.3 (2023-06-02)
### LPDC
  - re-publish public services with wrong language predicate
## 1.76.2 (2023-02-02)
### Toezicht
#### backend
  - hotfix cronPattern for toezicht, submission reports are now daily at 22:30
## 1.76.1 (2022-01-26)
### Toezicht
#### frontend
  - Fix the problem where the app freezes when selecting the "Belastingsreglement" option in the "Type reglement/verordening" field
## 1.76.0 (2022-01-26)
### General
#### frontend
  - modules to connect to external applications
  - bug birth date-picker
  - various improvements in semantic forms shared components
  - helpdesk link in person module
#### backend
  - Various maintenance operations, both data and code
  - Automatic submissions: migration to cogs:Jobs model, trackable submissions in the dashboard (if logged as bestuurseenheid through mock-login)
  - Various mock-login updates
  - Removal of the person reconcilation service
### Erediensten
#### inzendingen voor toezicht
  - Added besluit handhaven
  - Added Representatieve organen
  - Added forms Representatieve organen
  - Updated forms: meerjarenplanwijziging, advies budgetwijzing, advies jaarrekenig for Eredienstbestuur.
    - Extra selectbox component to link URI bestuureenheid of the advies.
  - Update publication logic for forms eredienst forms
  - Updates sync with kalliope
#### eredienst mandatarissen
  - perparation of read-only mode
  - permanent alert
  - address outside of flanders
#### eredienst bedienaren
  - perparation of read-only mode
  - address outside of flanders
#### data and data sharing
  - Added delta producer stream for eredienst-mandatarissen and bedienaren
  - Added Vandenbroele as vendor of RO's
### LPDC
 - textual updates
 - Prefill the "Authority" fields for blank service instances
 - Add support for fetching form data for conceptual public services (preparation for basic change notification feature)
 - Ensure language is correctly set in the database. (Will re-publish this information)
### Subsidies
 - open up nooddorpen and opknapwerk again
## 1.75.4 (2022-01-19)
### data migrations
- Some attachements were broken due to Kalliope, fixing these.
## 1.75.3 (2022-01-05)
### data migrations
- Remove: Bestuurslid van het bestuur van de eredienst
## 1.75.2 (2022-01-03)
### vendors
- vandenbroele was missing some centrale besturen.
## 1.75.1 (2022-12-20)
### submissions
 - Due to various issues after a batch sent in prod of automatic submission, we removed them to try again
## 1.75.0 (2022-12-16)
### Subsidies
 - fix buggy table Fietssubsidies
 - migration to skip optional step of climate subsidy 1 callup 2021
## 1.74.3 (2022-12-12)
### hofix
#### IPDC
  - Fixing required fields missing for PublicServices
  - Update publication, to compensation validation issues for nested required fields
## 1.74.2 (2022-12-06)
### hofix
#### IPDC
  - To prepare the export to ipdc, we remove the publication flag.
## 1.74.1 (2022-11-29)
### hofix
- attempt to fix load issues
## 1.74.0 (2022-11-29)
### lpdc
- allow multiple websites for procedure
- mandatory executing authority
- update code list:
  - remove rechtenverkenner vloket
  - limit doelgroep values + migrate existing
- fixes publish service: push back ipdc
### submissions
- updated LEKP forms: klimaattafels
### worship services
- several UI improvements eredienstmandaten & bedienaren
### data
- remove duplicate person Nijlen
- added betrokken lokale besturen
### chore
- fix file data model: `dct:created`
- update file service
- update dashboard
- deltanotifier files organisation
- update dumpfile producer: clean up old dumpes
## 1.73.0 (2022-10-27)
- bump file share service
- alignment identifiers for eredienstbesturen to enable ACM/IDM login
- Kerfabriek Vredegem
## 1.72.0 (2022-10-27)
- evolutions frontend loket eredienst-mandatarissen
- evolutions frontend loket eredienst-bedienaren
- producers worship submission
- upgrade prepare-submission-for-export: more fine grained rules
- lpdc evolutions: various bugfixes, re-publish flow and fixing publication issues
- fix unsaved address publications
- new vendor
- update vendor with 100+ bestuurseenheden
- various data fixes
- updated lpdc repor
- updated bestuursorganen erediensten
## 1.71.3 (2022-10-20)
- fix report fietssubsidies
## 1.71.2 (2022-10-19)
- fix an issue with the website field (LPDC)
## 1.71.1 (2022-10-18)
- fix an issue in the vendor-management app which prevented users from giving access rights to certain administrative units
## 1.71.0 (2022-10-17)
- adjust deadlines fietssubsidies
- update reports
## 1.70.1 (2022-10-12)
- hotfix climate subsidies: make sure old forms still render.
- hotfix status fietssubsidies: make sure status is set correctly
## 1.70.0 (2022-10-11)
- LEPK forms added
- Eredienst positions corrections
- Eredienst bestuursorganen corrections
- update fietssubsidie-reports
- less reports IPDC per day
## 1.69.1 (2022-10-06)
- lpdc-management ingests new organisations via IPDC
- typo in forms
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
