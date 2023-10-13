defmodule Dispatcher do
  use Matcher
  define_accept_types []

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   forward conn, path, "http://resource/themes/"
  # end

  match "/bestuurseenheden/*path" do
    forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorganen/*path" do
    forward conn, path, "http://cache/bestuursorganen/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end
  match "/fracties/*path" do
    forward conn, path, "http://cache/fracties/"
  end
  match "/fractietypes/*path" do
    forward conn, path, "http://cache/fractietypes/"
  end
  match "/geboortes/*path" do
    forward conn, path, "http://cache/geboortes/"
  end
  match "/lijsttypes/*path" do
    forward conn, path, "http://cache/lijsttypes/"
  end
  match "/kandidatenlijsten/*path" do
    forward conn, path, "http://cache/kandidatenlijsten/"
  end
  match "/lidmaatschappen/*path" do
    forward conn, path, "http://cache/lidmaatschappen/"
  end
  match "/mandaten/*path" do
    forward conn, path, "http://cache/mandaten/"
  end
  match "/bestuursfunctie-codes/*path" do
    forward conn, path, "http://cache/bestuursfunctie-codes/"
  end
  delete "/mandatarissen/:id" do
    forward conn, [], "http://mandataris-archive/" <> id <> "/archive"
  end
  match "/mandatarissen/*path" do
    forward conn, path, "http://cache/mandatarissen/"
  end
  match "/mandataris-status-codes/*path" do
    forward conn, path, "http://cache/mandataris-status-codes/"
  end
  match "/beleidsdomein-codes/*path" do
    forward conn, path, "http://cache/beleidsdomein-codes/"
  end
  match "/personen/*path" do
    forward conn, path, "http://cache/personen/"
  end
  match "/geslacht-codes/*path" do
    forward conn, path, "http://cache/geslacht-codes/"
  end

  match "/nationalities/*path" do
    Proxy.forward conn, path, "http://cache/nationalities/"
  end

  match "/identificatoren/*path" do
    forward conn, path, "http://cache/identificatoren/"
  end

  match "/tijdsintervallen/*path" do
    forward conn, path, "http://cache/tijdsintervallen/"
  end

  match "/mock/sessions/*path" do
    forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path" do
    forward conn, path, "http://login/sessions/"
  end
  match "/gebruikers/*path" do
    forward conn, path, "http://cache/gebruikers/"
  end
  match "/accounts/*path" do
    forward conn, path, "http://cache/accounts/"
  end

  match "/document-statuses/*path" do
    forward conn, path, "http://cache/document-statuses/"
  end
  get "/files/:id/download" do
    forward conn, [], "http://file/files/" <> id <> "/download"
  end
  get "/files/*path" do
    forward conn, path, "http://resource/files/"
  end
  patch "/files/*path" do
    forward conn, path, "http://resource/files/"
  end
  post "/files/*path" do
    forward conn, path, "http://file/files/"
  end
  # TODO: find all usage of this endpoint and replace it with `POST /files`
  # This is kept to maintain compatibility with code that uses the "old" endpoint.
  post "/file-service/files/*path" do
    forward conn, path, "http://file/files/"
  end
  delete "/files/*path" do
    forward conn, path, "http://file/files/"
  end
  match "/file-addresses/*path" do
    forward conn, path, "http://resource/file-addresses/"
  end
  match "/file-address-cache-statuses/*path" do
    forward conn, path, "http://resource/file-address-cache-statuses/"
  end
  post "/bbcdr-reports/*path" do
    forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end
  delete "/bbcdr-reports/*path" do
    forward conn, path, "http://resource/bbcdr-reports/"
  end
  get "/bbcdr-reports/*path" do
    forward conn, path, "http://resource/bbcdr-reports/"
  end
  patch "/bbcdr-reports/*path" do
    forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end
  post "/validation-executions/*path" do
    forward conn, path, "http://validation/executions/"
  end
  get "/validation-executions/*path" do
    forward conn, path, "http://resource/validation-executions/"
  end
  get "/validations/*path" do
    forward conn, path, "http://resource/validations/"
  end
  get "/validation-errors/*path" do
    forward conn, path, "http://resource/validation-errors/"
  end

  ###############################################################
  # dynamic-forms-domain.lisp
  ###############################################################
  match "/form-nodes/*path" do
    forward conn, path, "http://cache/form-nodes/"
  end
  match "/form-inputs/*path" do
    forward conn, path, "http://cache/form-inputs/"
  end
  match "/dynamic-subforms/*path" do
    forward conn, path, "http://cache/dynamic-subforms/"
  end

  match "/input-states/*path" do
    forward conn, path, "http://cache/input-states/"
  end

  ###############################################################
  # master-messages-domain.lisp
  ###############################################################
  match "/conversaties/*path" do
    forward conn, path, "http://resource/conversaties/"
  end

  match "/berichten/*path" do
    forward conn, path, "http://resource/berichten/"
  end

  ###############################################################
  # master-email-domain.lisp
  ###############################################################
  match "/mailboxes/*path" do
    forward conn, path, "http://resource/mailboxes/"
  end

  match "/mail-folders/*path" do
    forward conn, path, "http://resource/mail-folders/"
  end

  match "/emails/*path" do
    forward conn, path, "http://resource/emails/"
  end

  match "/email-headers/*path" do
    forward conn, path, "http://resource/email-headers/"
  end

  ###############################################################
  # master-log-domain.lisp
  ###############################################################

  match "/log-entries/*path" do
    forward conn, path, "http://resource/log-entries/"
  end

  match "/log-levels/*path" do
    forward conn, path, "http://resource/log-levels/"
  end

  match "/status-codes/*path" do
    forward conn, path, "http://resource/status-codes/"
  end

  match "/log-sources/*path" do
    forward conn, path, "http://resource/log-sources/"
  end

  match "/status-codes/*path" do
    forward conn, path, "http://resource/acm-idm-service-log-entries/"
  end

  #################################################################
  # slave leidinggevenden
  #################################################################
  match "/bestuursfuncties/*path" do
    forward conn, path, "http://cache/bestuursfuncties/"
  end

  match "/functionarissen/*path" do
    forward conn, path, "http://cache/functionarissen/"
  end

  match "/contact-punten/*path" do
    forward conn, path, "http://cache/contact-punten/"
  end

  match "/adressen/*path" do
    forward conn, path, "http://cache/adressen/"
  end

  match "/functionaris-status-codes/*path" do
    forward conn, path, "http://cache/functionaris-status-codes/"
  end

  #################################################################
  # personeelsdatabank
  #################################################################
  match "/employee-datasets/*path" do
    forward conn, path, "http://cache/employee-datasets/"
  end

  match "/employee-period-slices/*path" do
    forward conn, path, "http://cache/employee-period-slices/"
  end

  match "/employee-observations/*path" do
    forward conn, path, "http://cache/employee-observations/"
  end

  match "/employee-time-periods/*path" do
    forward conn, path, "http://cache/employee-time-periods/"
  end

  match "/educational-levels/*path" do
    forward conn, path, "http://cache/educational-levels/"
  end

  match "/working-time-categories/*path" do
    forward conn, path, "http://cache/working-time-categories/"
  end

  match "/employee-legal-statuses/*path" do
    forward conn, path, "http://cache/employee-legal-statuses/"
  end

  match "/employee-unit-measures/*path" do
    forward conn, path, "http://cache/employee-unit-measures/"
  end

  #################################################################
  # adressenregister
  #################################################################
  match "/adressenregister/*path" do
    forward conn, path, "http://adressenregister/"
  end

  #################################################################
  # Reports
  #################################################################
  match "/reports/*path" do
    forward conn, path, "http://resource/reports/"
  end

  #################################################################
  # Automatic submission
  #################################################################
  post "/melding/*path" do
    forward conn, path, "http://automatic-submission/melding"
  end

  post "/delete-melding" do
    Proxy.forward conn, [], "http://clean-up-submission/delete-melding"
  end

  #################################################################
  # Toezicht / supervision
  #################################################################

  match "/vendors/*path" do
    forward conn, path, "http://cache/vendors/"
  end

  match "/authenticity-types/*path" do
    forward conn, path, "http://cache/authenticity-types/"
  end

  match "/tax-types/*path" do
    forward conn, path, "http://cache/tax-types/"
  end

  match "/chart-of-accounts/*path" do
    forward conn, path, "http://cache/chart-of-accounts/"
  end

  match "/submission-document-statuses/*path" do
    forward conn, path, "http://cache/submission-document-statuses/"
  end

  match "/remote-urls/*path" do
    forward conn, path, "http://resource/remote-urls/"
  end

  get "/submission-forms/*path" do
    forward conn, path, "http://enrich-submission/submission-documents/"
  end

  delete "/submissions/*path" do
    forward conn, path, "http://clean-up-submission/submissions/"
  end

  put "/submissions/*path" do
    forward conn, path, "http://resource/submissions/"
  end

  patch "/submissions/*path" do
    forward conn, path, "http://resource/submissions/"
  end

  post "/submissions/*path" do
    forward conn, path, "http://resource/submissions/"
  end

  get "/submissions/*path" do
    forward conn, path, "http://resource/submissions/"
  end

  put "/submission-forms/:id/flatten" do
    forward conn, [], "http://toezicht-flattened-form-data-generator/submission-documents/" <> id <> "/flatten"
  end

  put "/submission-forms/:id" do
    forward conn, [], "http://validate-submission/submission-documents/" <> id
  end

  post "/submission-forms/:id/submit" do
    forward conn, [], "http://validate-submission/submission-documents/" <> id <> "/submit"
  end

  match "/submission-documents/*path" do
    forward conn, path, "http://cache/submission-documents/"
  end

  get "/form-data/*path" do
    forward conn, path, "http://resource/form-data/"
  end

  get "/concept-schemes/*path" do
    forward conn, path, "http://cache/concept-schemes/"
  end

  get "/concepts/*path" do
    forward conn, path, "http://cache/concepts/"
  end

  #################################################################
  # RRN SERVICE: person-uri-for-social-security-number-service
  #################################################################
  match "/rrn/*path" do
    forward conn, path, "http://person-uri-for-social-security-number/"
  end

  #################################################################
  # delta-files-share
  #################################################################
  get "/delta-files-share/download/*path" do
    forward conn, path, "http://delta-files-share/download/"
  end

  #################################################################
  # loket-mandatarissen sync
  #################################################################
  get "/sync/mandatarissen/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-mandatarissen/files/"
  end

  #################################################################
  # loket-leidinggevenden sync
  #################################################################
  get "/sync/leidinggevenden/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-leidinggevenden/files/"
  end

  #################################################################
  # loket-submissions sync
  #################################################################
  post "/sync/submissions/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/submissions/login/"
  end

  #################################################################
  # loket-submissions sync
  #################################################################
  get "/sync/submissions/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/submissions/files/"
  end

  #################################################################
  # worship-submissions sync
  #################################################################
  post "/sync/worship-submissions/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-worship-submissions/login/"
  end

  #################################################################
  # worship-submissions sync
  #################################################################
  get "/sync/worship-submissions/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-worship-submissions/files/"
  end

  #################################################################
  # worship-services-sensitive sync
  #################################################################
  post "/sync/worship-services-sensitive/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-ws-sensitive/login/"
  end

  get "/sync/worship-services-sensitive/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-ws-sensitive/files/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  post "/sync/persons-sensitive/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-persons-sensitive/login/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  get "/sync/persons-sensitive/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-persons-sensitive/files/"
  end

  #################################################################
  # loket-worship-services-sensitive sync
  #################################################################
  post "/sync/worship-services-sensitive-deltas/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-ws-sensitive/login/"
  end

  #################################################################
  # loket-worship-services-sensitive sync
  #################################################################
  get "/sync/worship-services-sensitive-deltas/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-ws-sensitive/files/"
  end

  #################################################################
  # loket-subsidies sync
  #################################################################
  post "/sync/subsidies/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-subsidies/login/"
  end

  #################################################################
  # loket-subsidies sync
  #################################################################
  get "/sync/subsidies/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer-subsidies/files/"
  end

  #################################################################
  # subsidy-applications: resources
  #################################################################

  match "/subsidy-measure-consumptions/*path" do
    forward conn, path, "http://cache/subsidy-measure-consumptions/"
  end

  match "/subsidy-measure-consumption-statuses/*path" do
    forward conn, path, "http://cache/subsidy-measure-consumption-statuses/"
  end

  match "/subsidy-requests/*path" do
    forward conn, path, "http://cache/subsidy-requests/"
  end

  match "/monetary-amounts/*path" do
    forward conn, path, "http://cache/monetary-amounts/"
  end

  match "/subsidy-measure-offers/*path" do
    forward conn, path, "http://cache/subsidy-measure-offers/"
  end

  match "/subsidy-measure-offer-series/*path" do
    forward conn, path, "http://cache/subsidy-measure-offer-series/"
  end

  match "/subsidy-application-flows/*path" do
    forward conn, path, "http://cache/subsidy-application-flows/"
  end

  match "/subsidy-application-flow-steps/*path" do
    forward conn, path, "http://cache/subsidy-application-flow-steps/"
  end

  match "/subsidy-procedural-steps/*path" do
    forward conn, path, "http://cache/subsidy-procedural-steps/"
  end

  match "/periods-of-time/*path" do
    forward conn, path, "http://cache/periods-of-time/"
  end

  match "/criteria/*path" do
    forward conn, path, "http://cache/criteria/"
  end

  match "/requirement-groups/*path" do
    forward conn, path, "http://cache/requirement-groups/"
  end

  match "/criterion-requirements/*path" do
    forward conn, path, "http://cache/criterion-requirements/"
  end

  match "/participations/*path" do
    forward conn, path, "http://cache/participations/"
  end

  match "/subsidy-application-forms/*path" do
    forward conn, path, "http://cache/subsidy-application-forms/"
  end

  #################################################################
  # subsidy-applications: custom API endpoints
  #################################################################

  match "/case-number-generator/*path" do
    forward conn, path, "http://case-number-generator/"
  end

  get "/management-active-form-file/*path" do
    forward conn, path, "http://subsidy-applications-management/active-form-file/"
  end

  get "/management-application-forms/*path" do
    forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  put "/management-application-forms/*path" do
    forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  delete "/management-application-forms/*path" do
    forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  post "/management-application-forms/:id/submit" do
    forward conn, [], "http://subsidy-applications-management/semantic-forms/" <> id <> "/submit"
  end

  match "/flow-management/*path" do
    forward conn, path, "http://subsidy-application-flow-management/flow/"
  end

  #################################################################
  # jobs
  #################################################################
  match "/jobs/*path" do
    forward conn, path, "http://cache/jobs/"
  end

  match "/tasks/*path" do
    forward conn, path, "http://cache/tasks/"
  end

  match "/data-containers/*path" do
    forward conn, path, "http://cache/data-containers/"
  end

  match "/job-errors/*path"  do
    forward conn, path, "http://cache/job-errors/"
  end

  #################################################################
  # DCAT
  #################################################################
  match "/datasets/*path" do
    forward conn, path, "http://cache/datasets/"
  end

  match "/distributions/*path" do
    forward conn, path, "http://cache/distributions/"
  end

  #################################################################
  # Bedienarenbeheer
  #################################################################
  match "/agents-in-position/*path" do
    forward conn, path, "http://cache/agents-in-position/"
  end

  match "/posts/*path" do
    forward conn, path, "http://cache/posts/"
  end

  match "/worship-mandatees/*path" do
    forward conn, path, "http://cache/worship-mandatees/"
  end

  match "/roles/*path" do
    forward conn, path, "http://cache/roles/"
  end

  match "/organizations/*path" do
    forward conn, path, "http://cache/organizations/"
  end

  match "/worship-administrative-units/*path" do
    forward conn, path, "http://cache/worship-administrative-units/"
  end

  match "/worship-services/*path" do
    forward conn, path, "http://cache/worship-services/"
  end

  match "/recognized-worship-types/*path" do
    forward conn, path, "http://cache/recognized-worship-types/"
  end

  match "/central-worship-services/*path" do
    forward conn, path, "http://cache/central-worship-services/"
  end

  match "/representative-bodies/*path" do
    forward conn, path, "http://cache/representative-bodies/"
  end

  match "/local-involvements/*path" do
    forward conn, path, "http://cache/local-involvements/"
  end

  match "/structured-identifiers/*path" do
    forward conn, path, "http://cache/structured-identifiers/"
  end

  match "/sites/*path" do
    forward conn, path, "http://cache/sites/"
  end

  match "/organization-status-codes/*path" do
    forward conn, path, "http://cache/organization-status-codes/"
  end

  match "/involvement-types/*path" do
    forward conn, path, "http://cache/involvement-types/"
  end

  match "/ministers/*path" do
    forward conn, path, "http://cache/ministers/"
  end

  match "/minister-conditions/*path" do
    forward conn, path, "http://cache/minister-conditions/"
  end

  match "/minister-positions/*path" do
    forward conn, path, "http://cache/minister-positions/"
  end

  match "/minister-position-functions/*path" do
    forward conn, path, "http://cache/minister-position-functions/"
  end

  match "/financing-codes/*path" do
    forward conn, path, "http://cache/financing-codes/"
  end

  match "/minister-condition-criterions/*path" do
    forward conn, path, "http://cache/minister-condition-criterions/"
  end

  match "/document-types-criterions/*path" do
    forward conn, path, "http://cache/document-types-criterions/"
  end

  match "/site-types/*path" do
    forward conn, path, "http://cache/site-types/"
  end

  #################################################################
  # Public Services - LPDC-IPDC: custom API endpoints
  #################################################################
  get "/lpdc-management/*path" do
    forward conn, path, "http://lpdc-management/semantic-forms/"
  end

  put "/lpdc-management/*path" do
    forward conn, path, "http://lpdc-management/semantic-forms/"
  end

  post "/lpdc-management/*path" do
    forward conn, path, "http://lpdc-management/semantic-forms/"
  end

  #################################################################
  # Public Services - LPDC-IPDC
  #################################################################
  match "/conceptual-public-services/*path" do
    forward conn, path, "http://cache/conceptual-public-services/"
  end

  match "/identifiers/*path" do
    forward conn, path, "http://cache/identifiers/"
  end

  post "/public-services/*path" do
    forward conn, path, "http://lpdc-management/public-services/"
  end

  delete "/public-services/*path" do
    forward conn, path, "http://lpdc-management/public-services/"
  end

  get "/public-services/*path" do
    forward conn, path, "http://resource/public-services/"  ## TODO: solve cache issue in frontend
  end

  match "/public-services/*path" do
    forward conn, path, "http://cache/public-services/"
  end

  match "/concept-display-configurations/*path" do
    forward conn, path, "http://cache/concept-display-configurations/"
  end

  #################################################################
  # Vendor Login for SPARQL endpoint
  #################################################################

  post "/vendor/login/*path" do
    Proxy.forward conn, path, "http://vendor-login/sessions"
  end

  delete "/vendor/logout" do
    Proxy.forward conn, [], "http://vendor-login/sessions/current"
  end

  #################################################################
  # Vendor SPARQL endpoint
  #################################################################

  # Not only POST. SPARQL via GET is also allowed.
  match "/vendor/sparql" do
    Proxy.forward conn, [], "http://sparql-authorization-wrapper/sparql"
  end

  match "/*_" do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
