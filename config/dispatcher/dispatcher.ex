defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    sparql: [ "application/sparql-results+json" ],
    any: [ "*/*" ]
  ]

  define_layers [ :static, :sparql, :api_services, :frontend, :resources, :not_found ]

  options "/*path", _ do
    conn
    |> Plug.Conn.put_resp_header( "access-control-allow-headers", "content-type,accept" )
    |> Plug.Conn.put_resp_header( "access-control-allow-methods", "*" )
    |> send_resp( 200, "{ \"message\": \"ok\" }" )
  end

  match "/bestuurseenheden/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end

  match "/werkingsgebieden/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end

  match "/bestuurseenheid-classificatie-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end

  match "/bestuursorganen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuursorganen/"
  end

  match "/bestuursorgaan-classificatie-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end

  match "/fracties/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/fracties/"
  end

  match "/fractietypes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/fractietypes/"
  end

  match "/geboortes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/geboortes/"
  end

  match "/lijsttypes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/lijsttypes/"
  end

  match "/kandidatenlijsten/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/kandidatenlijsten/"
  end

  match "/lidmaatschappen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/lidmaatschappen/"
  end

  match "/mandaten/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/mandaten/"
  end

  match "/bestuursfunctie-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuursfunctie-codes/"
  end

  delete "/mandatarissen/:id", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://mandataris-archive/" <> id <> "/archive"
  end
  
  match "/mandatarissen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/mandatarissen/"
  end

  match "/mandataris-status-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/mandataris-status-codes/"
  end

  match "/beleidsdomein-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/beleidsdomein-codes/"
  end

  match "/personen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/personen/"
  end

  match "/geslacht-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/geslacht-codes/"
  end
  
  match "/identificatoren/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/identificatoren/"
  end

  match "/tijdsintervallen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/tijdsintervallen/"
  end

  match "/mock/sessions/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end

  match "/sessions/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  match "/gebruikers/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/gebruikers/"
  end

  match "/accounts/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/document-statuses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/document-statuses/"
  end

  get "/files/:id/download", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  get "/files/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/files/"
  end

  patch "/files/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/files/"
  end

  post "/file-service/files/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://file/files/"
  end

  match "/file-addresses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/file-addresses/"
  end

  match "/file-address-cache-statuses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/file-address-cache-statuses/"
  end

  post "/bbcdr-reports/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end

  delete "/bbcdr-reports/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end

  get "/bbcdr-reports/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end

  patch "/bbcdr-reports/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end

  post "/validation-executions/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://validation/executions/"
  end

  get "/validation-executions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/validation-executions/"
  end

  get "/validations/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/validations/"
  end

  get "/validation-errors/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/validation-errors/"
  end

  ###############################################################
  # dynamic-forms-domain.lisp
  ###############################################################
  match "/form-nodes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/form-nodes/"
  end

  match "/form-inputs/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/form-inputs/"
  end

  match "/dynamic-subforms/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/dynamic-subforms/"
  end

  match "/input-states/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/input-states/"
  end

  ###############################################################
  # master-messages-domain.lisp
  ###############################################################
  match "/conversaties/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/conversaties/"
  end

  match "/berichten/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/berichten/"
  end

  ###############################################################
  # master-email-domain.lisp
  ###############################################################
  match "/mailboxes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/mailboxes/"
  end

  match "/mail-folders/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/mail-folders/"
  end

  match "/emails/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/emails/"
  end

  match "/email-headers/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/email-headers/"
  end

  ###############################################################
  # master-log-domain.lisp
  ###############################################################

  match "/log-entries/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/log-entries/"
  end

  match "/log-levels/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/log-levels/"
  end

  match "/status-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/status-codes/"
  end

  match "/log-sources/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/log-sources/"
  end

  match "/status-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/acm-idm-service-log-entries/"
  end

  #################################################################
  # slave leidinggevenden
  #################################################################
  match "/bestuursfuncties/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/bestuursfuncties/"
  end

  match "/functionarissen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/functionarissen/"
  end

  match "/contact-punten/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/contact-punten/"
  end

  match "/adressen/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/adressen/"
  end

  match "/functionaris-status-codes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/functionaris-status-codes/"
  end

  #################################################################
  # personeelsdatabank
  #################################################################
  match "/employee-datasets/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-datasets/"
  end

  match "/employee-period-slices/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-period-slices/"
  end

  match "/employee-observations/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-observations/"
  end

  match "/employee-time-periods/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-time-periods/"
  end

  match "/educational-levels/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/educational-levels/"
  end

  match "/working-time-categories/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/working-time-categories/"
  end

  match "/employee-legal-statuses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-legal-statuses/"
  end

  match "/employee-unit-measures/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/employee-unit-measures/"
  end

  #################################################################
  # adressenregister
  #################################################################
  match "/adressenregister/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://adressenregister/"
  end

  #################################################################
  # Reports
  #################################################################
  match "/reports/*path", %{ layer: :resources, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://resource/reports/"
  end

  #################################################################
  # Automatic submission
  #################################################################
  match "/melding/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://automatic-submission/melding"
  end

  #################################################################
  # Toezicht / supervision
  #################################################################

  match "/vendors/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/vendors/"
  end

  match "/authenticity-types/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/authenticity-types/"
  end

  match "/tax-types/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/tax-types/"
  end

  match "/chart-of-accounts/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/chart-of-accounts/"
  end

  match "/submission-document-statuses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/submission-document-statuses/"
  end

  match "/remote-urls/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/remote-urls/"
  end

  get "/submission-forms/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://enrich-submission/submission-documents/"
  end

  delete "/submissions/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://clean-up-submission/submissions/"
  end

  put "/submissions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  patch "/submissions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  post "/submissions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  get "/submissions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  put "/submission-forms/:id/flatten", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://toezicht-flattened-form-data-generator/submission-documents/" <> id <> "/flatten"
  end

  put "/submission-forms/:id", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://validate-submission/submission-documents/" <> id
  end

  post "/submission-forms/:id/submit", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://validate-submission/submission-documents/" <> id <> "/submit"
  end

  match "/submission-documents/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/submission-documents/"
  end

  get "/form-data/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://resource/form-data/"
  end

  get "/concept-schemes/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/concept-schemes/"
  end

  get "/concepts/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/concepts/"
  end

  #################################################################
  # RRN SERVICE: person-uri-for-social-security-number-service
  #################################################################
  match "/rrn/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://person-uri-for-social-security-number/"
  end

  #################################################################
  # loket-mandatarissen sync
  #################################################################
  get "/sync/mandatarissen/files/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://delta-producer-json-diff-file-publisher-mandatarissen/files/"
  end

  #################################################################
  # loket-leidinggevenden sync
  #################################################################
  get "/sync/leidinggevenden/files/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://delta-producer-json-diff-file-publisher-leidinggevenden/files/"
  end

  #################################################################
  # subsidy-applications: resources
  #################################################################

  match "/subsidy-measure-consumptions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-measure-consumptions/"
  end

  match "/subsidy-measure-consumption-statuses/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-measure-consumption-statuses/"
  end

  match "/subsidy-requests/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-requests/"
  end

  match "/monetary-amounts/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/monetary-amounts/"
  end

  match "/subsidy-measure-offers/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-measure-offers/"
  end

  match "/subsidy-measure-offer-series/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-measure-offer-series/"
  end

  match "/subsidy-application-flows/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-application-flows/"
  end

  match "/subsidy-application-flow-steps/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-application-flow-steps/"
  end

  match "/subsidy-procedural-steps/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-procedural-steps/"
  end

  match "/periods-of-time/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/periods-of-time/"
  end

  match "/criteria/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/criteria/"
  end

  match "/requirement-groups/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/requirement-groups/"
  end

  match "/criterion-requirements/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/criterion-requirements/"
  end

  match "/participations/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/participations/"
  end

  match "/subsidy-application-forms/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/subsidy-application-forms/"
  end

  #################################################################
  # subsidy-applications: custom API endpoints
  #################################################################

  match "/case-number-generator/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://case-number-generator/"
  end

  get "/management-active-form-file/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://subsidy-applications-management/active-form-file/"
  end

  get "/management-application-forms/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  put "/management-application-forms/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  delete "/management-application-forms/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  post "/management-application-forms/:id/submit", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, [], "http://subsidy-applications-management/semantic-forms/" <> id <> "/submit"
  end

  match "/flow-management/*path", %{ layer: :api_services, accept: %{ any: true } } do
    Proxy.forward conn, path, "http://subsidy-application-flow-management/flow/"
  end

  #################################################################
  # jobs
  #################################################################
  match "/jobs/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/jobs/"
  end

  match "/tasks/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/tasks/"
  end

  match "/data-containers/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/data-containers/"
  end

  match "/job-errors/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/job-errors/"
  end

  #################################################################
  # DCAT
  #################################################################
  match "/datasets/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/datasets/"
  end

  match "/distributions/*path", %{ layer: :resources, accept: %{ json: true } } do
    Proxy.forward conn, path, "http://cache/distributions/"
  end

  match "/*_path", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
