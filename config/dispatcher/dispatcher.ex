defmodule Dispatcher do
  use Plug.Router

  def start(_argv) do
    port = 80
    IO.puts "Starting Plug with Cowboy on port #{port}"
    Plug.Adapters.Cowboy.http __MODULE__, [], port: port
    :timer.sleep(:infinity)
  end

  plug Plug.Logger
  plug :match
  plug :dispatch

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end

  match "/bestuurseenheden/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorganen/*path" do
    Proxy.forward conn, path, "http://cache/bestuursorganen/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end
  match "/fracties/*path" do
    Proxy.forward conn, path, "http://cache/fracties/"
  end
  match "/fractietypes/*path" do
    Proxy.forward conn, path, "http://cache/fractietypes/"
  end
  match "/geboortes/*path" do
    Proxy.forward conn, path, "http://cache/geboortes/"
  end
  match "/lijsttypes/*path" do
    Proxy.forward conn, path, "http://cache/lijsttypes/"
  end
  match "/kandidatenlijsten/*path" do
    Proxy.forward conn, path, "http://cache/kandidatenlijsten/"
  end
  match "/lidmaatschappen/*path" do
    Proxy.forward conn, path, "http://cache/lidmaatschappen/"
  end
  match "/mandaten/*path" do
    Proxy.forward conn, path, "http://cache/mandaten/"
  end
  match "/bestuursfunctie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuursfunctie-codes/"
  end
  delete "/mandatarissen/:id" do
    Proxy.forward conn, [], "http://mandataris-archive/" <> id <> "/archive"
  end
  match "/mandatarissen/*path" do
    Proxy.forward conn, path, "http://cache/mandatarissen/"
  end
  match "/mandataris-status-codes/*path" do
    Proxy.forward conn, path, "http://cache/mandataris-status-codes/"
  end
  match "/beleidsdomein-codes/*path" do
    Proxy.forward conn, path, "http://cache/beleidsdomein-codes/"
  end
  match "/personen/*path" do
    Proxy.forward conn, path, "http://cache/personen/"
  end
  match "/geslacht-codes/*path" do
    Proxy.forward conn, path, "http://cache/geslacht-codes/"
  end
  match "/identificatoren/*path" do
    Proxy.forward conn, path, "http://cache/identificatoren/"
  end

  match "/tijdsintervallen/*path" do
    Proxy.forward conn, path, "http://cache/tijdsintervallen/"
  end

  match "/mock/sessions/*path" do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path" do
    Proxy.forward conn, path, "http://login/sessions/"
  end
  match "/gebruikers/*path" do
    Proxy.forward conn, path, "http://cache/gebruikers/"
  end
  match "/accounts/*path" do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match "/document-statuses/*path" do
    Proxy.forward conn, path, "http://cache/document-statuses/"
  end
  get "/files/:id/download" do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end
  get "/files/*path" do
    Proxy.forward conn, path, "http://resource/files/"
  end
  patch "/files/*path" do
    Proxy.forward conn, path, "http://resource/files/"
  end
  post "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end
  # TODO: find all usage of this endpoint and replace it with `POST /files`
  # This is kept to maintain compatibility with code that uses the "old" endpoint.
  post "/file-service/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end
  delete "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end
  match "/file-addresses/*path" do
    Proxy.forward conn, path, "http://resource/file-addresses/"
  end
  match "/file-address-cache-statuses/*path" do
    Proxy.forward conn, path, "http://resource/file-address-cache-statuses/"
  end
  post "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end
  delete "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end
  get "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end
  patch "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://create-bbcdr/bbcdr-reports/"
  end
  post "/validation-executions/*path" do
    Proxy.forward conn, path, "http://validation/executions/"
  end
  get "/validation-executions/*path" do
    Proxy.forward conn, path, "http://resource/validation-executions/"
  end
  get "/validations/*path" do
    Proxy.forward conn, path, "http://resource/validations/"
  end
  get "/validation-errors/*path" do
    Proxy.forward conn, path, "http://resource/validation-errors/"
  end

  ###############################################################
  # dynamic-forms-domain.lisp
  ###############################################################
  match "/form-nodes/*path" do
    Proxy.forward conn, path, "http://cache/form-nodes/"
  end
  match "/form-inputs/*path" do
    Proxy.forward conn, path, "http://cache/form-inputs/"
  end
  match "/dynamic-subforms/*path" do
    Proxy.forward conn, path, "http://cache/dynamic-subforms/"
  end

  match "/input-states/*path" do
    Proxy.forward conn, path, "http://cache/input-states/"
  end

  ###############################################################
  # master-messages-domain.lisp
  ###############################################################
  match "/conversaties/*path" do
    Proxy.forward conn, path, "http://resource/conversaties/"
  end

  match "/berichten/*path" do
    Proxy.forward conn, path, "http://resource/berichten/"
  end

  ###############################################################
  # master-email-domain.lisp
  ###############################################################
  match "/mailboxes/*path" do
    Proxy.forward conn, path, "http://resource/mailboxes/"
  end

  match "/mail-folders/*path" do
    Proxy.forward conn, path, "http://resource/mail-folders/"
  end

  match "/emails/*path" do
    Proxy.forward conn, path, "http://resource/emails/"
  end

  match "/email-headers/*path" do
    Proxy.forward conn, path, "http://resource/email-headers/"
  end

  ###############################################################
  # master-log-domain.lisp
  ###############################################################

  match "/log-entries/*path" do
    Proxy.forward conn, path, "http://resource/log-entries/"
  end

  match "/log-levels/*path" do
    Proxy.forward conn, path, "http://resource/log-levels/"
  end

  match "/status-codes/*path" do
    Proxy.forward conn, path, "http://resource/status-codes/"
  end

  match "/log-sources/*path" do
    Proxy.forward conn, path, "http://resource/log-sources/"
  end

  match "/status-codes/*path" do
    Proxy.forward conn, path, "http://resource/acm-idm-service-log-entries/"
  end

  #################################################################
  # slave leidinggevenden
  #################################################################
  match "/bestuursfuncties/*path" do
    Proxy.forward conn, path, "http://cache/bestuursfuncties/"
  end

  match "/functionarissen/*path" do
    Proxy.forward conn, path, "http://cache/functionarissen/"
  end

  match "/contact-punten/*path" do
    Proxy.forward conn, path, "http://cache/contact-punten/"
  end

  match "/adressen/*path" do
    Proxy.forward conn, path, "http://cache/adressen/"
  end

  match "/functionaris-status-codes/*path" do
    Proxy.forward conn, path, "http://cache/functionaris-status-codes/"
  end

  #################################################################
  # personeelsdatabank
  #################################################################
  match "/employee-datasets/*path" do
    Proxy.forward conn, path, "http://cache/employee-datasets/"
  end

  match "/employee-period-slices/*path" do
    Proxy.forward conn, path, "http://cache/employee-period-slices/"
  end

  match "/employee-observations/*path" do
    Proxy.forward conn, path, "http://cache/employee-observations/"
  end

  match "/employee-time-periods/*path" do
    Proxy.forward conn, path, "http://cache/employee-time-periods/"
  end

  match "/educational-levels/*path" do
    Proxy.forward conn, path, "http://cache/educational-levels/"
  end

  match "/working-time-categories/*path" do
    Proxy.forward conn, path, "http://cache/working-time-categories/"
  end

  match "/employee-legal-statuses/*path" do
    Proxy.forward conn, path, "http://cache/employee-legal-statuses/"
  end

  match "/employee-unit-measures/*path" do
    Proxy.forward conn, path, "http://cache/employee-unit-measures/"
  end

  #################################################################
  # adressenregister
  #################################################################
  match "/adressenregister/*path" do
    Proxy.forward conn, path, "http://adressenregister/"
  end

  #################################################################
  # Reports
  #################################################################
  match "/reports/*path" do
    Proxy.forward conn, path, "http://resource/reports/"
  end

  #################################################################
  # Automatic submission
  #################################################################
  match "/melding/*path" do
    Proxy.forward conn, path, "http://automatic-submission/melding"
  end

  #################################################################
  # Toezicht / supervision
  #################################################################

  match "/vendors/*path" do
    Proxy.forward conn, path, "http://cache/vendors/"
  end

  match "/authenticity-types/*path" do
    Proxy.forward conn, path, "http://cache/authenticity-types/"
  end

  match "/tax-types/*path" do
    Proxy.forward conn, path, "http://cache/tax-types/"
  end

  match "/chart-of-accounts/*path" do
    Proxy.forward conn, path, "http://cache/chart-of-accounts/"
  end

  match "/submission-document-statuses/*path" do
    Proxy.forward conn, path, "http://cache/submission-document-statuses/"
  end

  match "/remote-urls/*path" do
    Proxy.forward conn, path, "http://resource/remote-urls/"
  end

  get "/submission-forms/*path" do
    Proxy.forward conn, path, "http://enrich-submission/submission-documents/"
  end

  delete "/submissions/*path" do
    Proxy.forward conn, path, "http://clean-up-submission/submissions/"
  end

  put "/submissions/*path" do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  patch "/submissions/*path" do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  post "/submissions/*path" do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  get "/submissions/*path" do
    Proxy.forward conn, path, "http://resource/submissions/"
  end

  put "/submission-forms/:id/flatten" do
    Proxy.forward conn, [], "http://toezicht-flattened-form-data-generator/submission-documents/" <> id <> "/flatten"
  end

  put "/submission-forms/:id" do
    Proxy.forward conn, [], "http://validate-submission/submission-documents/" <> id
  end

  post "/submission-forms/:id/submit" do
    Proxy.forward conn, [], "http://validate-submission/submission-documents/" <> id <> "/submit"
  end

  match "/submission-documents/*path" do
    Proxy.forward conn, path, "http://cache/submission-documents/"
  end

  get "/form-data/*path" do
    Proxy.forward conn, path, "http://resource/form-data/"
  end

  get "/concept-schemes/*path" do
    Proxy.forward conn, path, "http://cache/concept-schemes/"
  end

  get "/concepts/*path" do
    Proxy.forward conn, path, "http://cache/concepts/"
  end

  #################################################################
  # RRN SERVICE: person-uri-for-social-security-number-service
  #################################################################
  match "/rrn/*path" do
    Proxy.forward conn, path, "http://person-uri-for-social-security-number/"
  end

  #################################################################
  # delta-files-share
  #################################################################
  get "/delta-files-share/download/*path" do
    Proxy.forward conn, path, "http://delta-files-share/download/"
  end

  #################################################################
  # loket-mandatarissen sync
  #################################################################
  get "/sync/mandatarissen/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-mandatarissen/files/"
  end

  #################################################################
  # loket-leidinggevenden sync
  #################################################################
  get "/sync/leidinggevenden/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-leidinggevenden/files/"
  end

  #################################################################
  # loket-submissions sync
  #################################################################
  post "/sync/submissions/login/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-submissions/login/"
  end

  #################################################################
  # loket-submissions sync
  #################################################################
  get "/sync/submissions/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-submissions/files/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  post "/sync/persons-sensitive/login/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-persons-sensitive/login/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  get "/sync/persons-sensitive/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-persons-sensitive/files/"
  end

  #################################################################
  # loket-subsidies sync
  #################################################################
  post "/sync/subsidies/login/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-subsidies/login/"
  end

  #################################################################
  # loket-subsidies sync
  #################################################################
  get "/sync/subsidies/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer-subsidies/files/"
  end

  #################################################################
  # subsidy-applications: resources
  #################################################################

  match "/subsidy-measure-consumptions/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-measure-consumptions/"
  end

  match "/subsidy-measure-consumption-statuses/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-measure-consumption-statuses/"
  end

  match "/subsidy-requests/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-requests/"
  end

  match "/monetary-amounts/*path" do
    Proxy.forward conn, path, "http://cache/monetary-amounts/"
  end

  match "/subsidy-measure-offers/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-measure-offers/"
  end

  match "/subsidy-measure-offer-series/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-measure-offer-series/"
  end

  match "/subsidy-application-flows/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-application-flows/"
  end

  match "/subsidy-application-flow-steps/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-application-flow-steps/"
  end

  match "/subsidy-procedural-steps/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-procedural-steps/"
  end

  match "/periods-of-time/*path" do
    Proxy.forward conn, path, "http://cache/periods-of-time/"
  end

  match "/criteria/*path" do
    Proxy.forward conn, path, "http://cache/criteria/"
  end

  match "/requirement-groups/*path" do
    Proxy.forward conn, path, "http://cache/requirement-groups/"
  end

  match "/criterion-requirements/*path" do
    Proxy.forward conn, path, "http://cache/criterion-requirements/"
  end

  match "/participations/*path" do
    Proxy.forward conn, path, "http://cache/participations/"
  end

  match "/subsidy-application-forms/*path" do
    Proxy.forward conn, path, "http://cache/subsidy-application-forms/"
  end

  #################################################################
  # subsidy-applications: custom API endpoints
  #################################################################

  match "/case-number-generator/*path" do
    Proxy.forward conn, path, "http://case-number-generator/"
  end

  get "/management-active-form-file/*path" do
    Proxy.forward conn, path, "http://subsidy-applications-management/active-form-file/"
  end

  get "/management-application-forms/*path" do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  put "/management-application-forms/*path" do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  delete "/management-application-forms/*path" do
    Proxy.forward conn, path, "http://subsidy-applications-management/semantic-forms/"
  end

  post "/management-application-forms/:id/submit" do
    Proxy.forward conn, [], "http://subsidy-applications-management/semantic-forms/" <> id <> "/submit"
  end

  match "/flow-management/*path" do
    Proxy.forward conn, path, "http://subsidy-application-flow-management/flow/"
  end

  #################################################################
  # jobs
  #################################################################
  match "/jobs/*path" do
    Proxy.forward conn, path, "http://cache/jobs/"
  end

  match "/tasks/*path" do
    Proxy.forward conn, path, "http://cache/tasks/"
  end

  match "/data-containers/*path" do
    Proxy.forward conn, path, "http://cache/data-containers/"
  end

  match "/job-errors/*path"  do
    Proxy.forward conn, path, "http://cache/job-errors/"
  end

  #################################################################
  # DCAT
  #################################################################
  match "/datasets/*path" do
    Proxy.forward conn, path, "http://cache/datasets/"
  end

  match "/distributions/*path" do
    Proxy.forward conn, path, "http://cache/distributions/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
