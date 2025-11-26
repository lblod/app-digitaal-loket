##########################################################################
# WARNING: mu-cache is temporarily disabled.
# sparql-parser is (currently) too powerful for resource to allow it to properly
# clear the caches. 
##########################################################################
defmodule Dispatcher do
  use Matcher

  define_accept_types [
    html: ["text/html", "application/xhtml+html"],
    json: ["application/json", "application/vnd.api+json"],
    upload: ["multipart/form-data"],
    any: [ "*/*" ],
  ]

  @html %{ accept: %{ html: true } }
  @json %{ accept: %{ json: true } }
  @upload %{ accept: %{ upload: true } }
  @any %{ accept: %{ any: true } }

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
    forward conn, path, "http://resource/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    forward conn, path, "http://resource/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    forward conn, path, "http://resource/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorganen/*path" do
    forward conn, path, "http://resource/bestuursorganen/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    forward conn, path, "http://resource/bestuursorgaan-classificatie-codes/"
  end
  match "/fracties/*path" do
    forward conn, path, "http://resource/fracties/"
  end
  match "/fractietypes/*path" do
    forward conn, path, "http://resource/fractietypes/"
  end
  match "/geboortes/*path" do
    forward conn, path, "http://resource/geboortes/"
  end
  match "/lijsttypes/*path" do
    forward conn, path, "http://resource/lijsttypes/"
  end
  match "/kandidatenlijsten/*path" do
    forward conn, path, "http://resource/kandidatenlijsten/"
  end
  match "/lidmaatschappen/*path" do
    forward conn, path, "http://resource/lidmaatschappen/"
  end
  match "/mandaten/*path" do
    forward conn, path, "http://resource/mandaten/"
  end
  match "/bestuursfunctie-codes/*path" do
    forward conn, path, "http://resource/bestuursfunctie-codes/"
  end
  delete "/mandatarissen/:id" do
    forward conn, [], "http://mandataris-archive/" <> id <> "/archive"
  end
  match "/mandatarissen/*path" do
    forward conn, path, "http://resource/mandatarissen/"
  end
  match "/mandataris-status-codes/*path" do
    forward conn, path, "http://resource/mandataris-status-codes/"
  end
  match "/beleidsdomein-codes/*path" do
    forward conn, path, "http://resource/beleidsdomein-codes/"
  end
  match "/personen/*path" do
    forward conn, path, "http://resource/personen/"
  end
  match "/geslacht-codes/*path" do
    forward conn, path, "http://resource/geslacht-codes/"
  end

  match "/nationalities/*path" do
    Proxy.forward conn, path, "http://resource/nationalities/"
  end

  match "/identificatoren/*path" do
    forward conn, path, "http://resource/identificatoren/"
  end

  match "/tijdsintervallen/*path" do
    forward conn, path, "http://resource/tijdsintervallen/"
  end

  match "/mock/sessions/*path" do
    forward conn, path, "http://mocklogin/sessions/"
  end

  match "/impersonations/*path" do
    forward conn, path, "http://impersonation/impersonations/"
  end

  match "/gebruikers/*path" do
    forward conn, path, "http://resource/gebruikers/"
  end
  match "/accounts/*path" do
    forward conn, path, "http://resource/accounts/"
  end

  match "/document-statuses/*path" do
    forward conn, path, "http://resource/document-statuses/"
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
  match "/file-address-resource-statuses/*path" do
    forward conn, path, "http://resource/file-address-resource-statuses/"
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
    forward conn, path, "http://resource/bestuursfuncties/"
  end

  match "/functionarissen/*path" do
    forward conn, path, "http://resource/functionarissen/"
  end

  match "/contact-punten/*path" do
    forward conn, path, "http://resource/contact-punten/"
  end

  match "/adressen/*path" do
    forward conn, path, "http://resource/adressen/"
  end

  match "/functionaris-status-codes/*path" do
    forward conn, path, "http://resource/functionaris-status-codes/"
  end

  #################################################################
  # personeelsdatabank
  #################################################################
  match "/employee-datasets/*path" do
    forward conn, path, "http://resource/employee-datasets/"
  end

  match "/employee-period-slices/*path" do
    forward conn, path, "http://resource/employee-period-slices/"
  end

  match "/employee-observations/*path" do
    forward conn, path, "http://resource/employee-observations/"
  end

  match "/employee-time-periods/*path" do
    forward conn, path, "http://resource/employee-time-periods/"
  end

  match "/educational-levels/*path" do
    forward conn, path, "http://resource/educational-levels/"
  end

  match "/working-time-categories/*path" do
    forward conn, path, "http://resource/working-time-categories/"
  end

  match "/employee-legal-statuses/*path" do
    forward conn, path, "http://resource/employee-legal-statuses/"
  end

  match "/employee-unit-measures/*path" do
    forward conn, path, "http://resource/employee-unit-measures/"
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

  # NOTE: use resources! This service has `links: - virtuoso:datase`, because
  # it is heavy on the database.
  match "/reports/*path", @json do
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

  match "/vendors/*path", @json do
    forward conn, path, "http://resource/vendors/"
  end

  match "/authenticity-types/*path" do
    forward conn, path, "http://resource/authenticity-types/"
  end

  match "/tax-types/*path" do
    forward conn, path, "http://resource/tax-types/"
  end

  match "/chart-of-accounts/*path" do
    forward conn, path, "http://resource/chart-of-accounts/"
  end

  match "/submission-document-statuses/*path" do
    forward conn, path, "http://resource/submission-document-statuses/"
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
    forward conn, path, "http://resource/submission-documents/"
  end

  get "/form-data/*path" do
    forward conn, path, "http://resource/form-data/"
  end

  get "/concept-schemes/*path" do
    forward conn, path, "http://resource/concept-schemes/"
  end

  get "/concepts/*path" do
    forward conn, path, "http://resource/concepts/"
  end

  get "/worship-decisions-cross-reference/search-documents/*path" do
    forward conn, path, "http://worship-decisions-cross-reference/search-documents/"
  end

  get "/worship-decisions-cross-reference/document-information/*path" do
    forward conn, path, "http://worship-decisions-cross-reference/document-information/"
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
  post "/sync/mandatarissen/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/mandatarissen/login/"
  end

  get "/sync/mandatarissen/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/mandatarissen/files/"
  end

  #################################################################
  # loket-leidinggevenden sync
  #################################################################
  post "/sync/leidinggevenden/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/leidinggevenden/login/"
  end

  get "/sync/leidinggevenden/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/leidinggevenden/files/"
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
    forward conn, path, "http://delta-producer-publication-graph-maintainer/worship-submissions/login/"
  end

  #################################################################
  # worship-submissions sync
  #################################################################
  get "/sync/worship-submissions/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer/worship-submissions/files/"
  end

  #################################################################
  # worship-services-sensitive sync
  #################################################################
  post "/sync/worship-services-sensitive/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/worship-services-sensitive/login/"
  end

  get "/sync/worship-services-sensitive/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer/worship-services-sensitive/files/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  post "/sync/persons-sensitive/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/persons-sensitive/login/"
  end

  #################################################################
  # loket-persons-sensitive sync
  #################################################################
  get "/sync/persons-sensitive/files/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/persons-sensitive/files/"
  end

  #################################################################
  # Vendor-Management sync
  #################################################################
  post "/sync/vendor-management/login/*path" do
    forward conn, path, "http://delta-producer-publication-graph-maintainer/vendor-management/login/"
  end

  get "/sync/vendor-management/files/*path" do
    Proxy.forward conn, path, "http://delta-producer-publication-graph-maintainer/vendor-management/files/"
  end

  #################################################################
  # jobs
  #################################################################
  match "/jobs/*path", @json do
    forward conn, path, "http://resource/jobs/"
  end

  match "/tasks/*path" do
    forward conn, path, "http://resource/tasks/"
  end

  match "/data-containers/*path" do
    forward conn, path, "http://resource/data-containers/"
  end

  match "/job-errors/*path"  do
    forward conn, path, "http://resource/job-errors/"
  end

  #################################################################
  # DCAT
  #################################################################
  match "/datasets/*path" do
    forward conn, path, "http://resource/datasets/"
  end

  match "/distributions/*path" do
    forward conn, path, "http://resource/distributions/"
  end

  #################################################################
  # Bedienarenbeheer
  #################################################################
  match "/agents-in-position/*path" do
    forward conn, path, "http://resource/agents-in-position/"
  end

  match "/posts/*path" do
    forward conn, path, "http://resource/posts/"
  end

  match "/worship-mandatees/*path" do
    forward conn, path, "http://resource/worship-mandatees/"
  end

  match "/roles/*path" do
    forward conn, path, "http://resource/roles/"
  end

  match "/organizations/*path" do
    forward conn, path, "http://resource/organizations/"
  end

  match "/worship-administrative-units/*path" do
    forward conn, path, "http://resource/worship-administrative-units/"
  end

  match "/worship-services/*path" do
    forward conn, path, "http://resource/worship-services/"
  end

  match "/recognized-worship-types/*path" do
    forward conn, path, "http://resource/recognized-worship-types/"
  end

  match "/central-worship-services/*path" do
    forward conn, path, "http://resource/central-worship-services/"
  end

  match "/representative-bodies/*path" do
    forward conn, path, "http://resource/representative-bodies/"
  end

  match "/local-involvements/*path" do
    forward conn, path, "http://resource/local-involvements/"
  end

  match "/structured-identifiers/*path" do
    forward conn, path, "http://resource/structured-identifiers/"
  end

  match "/sites/*path" do
    forward conn, path, "http://resource/sites/"
  end

  match "/organization-status-codes/*path" do
    forward conn, path, "http://resource/organization-status-codes/"
  end

  match "/involvement-types/*path" do
    forward conn, path, "http://resource/involvement-types/"
  end

  match "/ministers/*path" do
    forward conn, path, "http://resource/ministers/"
  end

  match "/minister-conditions/*path" do
    forward conn, path, "http://resource/minister-conditions/"
  end

  match "/minister-positions/*path" do
    forward conn, path, "http://resource/minister-positions/"
  end

  match "/minister-position-functions/*path" do
    forward conn, path, "http://resource/minister-position-functions/"
  end

  match "/financing-codes/*path" do
    forward conn, path, "http://resource/financing-codes/"
  end

  match "/minister-condition-criterions/*path" do
    forward conn, path, "http://resource/minister-condition-criterions/"
  end

  match "/document-types-criterions/*path" do
    forward conn, path, "http://resource/document-types-criterions/"
  end

  match "/site-types/*path" do
    forward conn, path, "http://resource/site-types/"
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

  #################################################################
  # Berichtencentrum: melding
  #################################################################
  post "/vendor/berichtencentrum/melding/*path" do
    forward conn, path, "http://berichtencentrum-melding/melding"
  end

  #################################################################
  # Dashboard
  #################################################################

  # Login

  match "/sessions/*path", %{ reverse_host: ["dashboard" | _rest] } do
    forward conn, path, "http://login-dashboard/sessions/"
  end

  # Frontend

  get "/assets/*path",  %{ accept: %{ any: true }, reverse_host: ["dashboard" | _rest] }  do
    forward conn, path, "http://dashboard/assets/"
  end

  get "/@appuniversum/*path", %{ accept: %{ any: true }, reverse_host: ["dashboard" | _rest] } do
    forward conn, path, "http://dashboard/@appuniversum/"
  end

  match "/*_path", %{ accept: %{ html: true }, reverse_host: ["dashboard" | _rest] } do
    forward conn, [], "http://dashboard/index.html"
  end

  # Resources

  match "/remote-data-objects/*path" do
    forward conn, path, "http://resource/remote-data-objects/"
  end

  #################################################################
  # Vendor Management
  #################################################################

  # Login

  match "/sessions/*path", %{ reverse_host: ["vendor-management" | _rest] } do
    forward conn, path, "http://login-vendor-management/sessions/"
  end

  # Frontend

  get "/assets/*path",  %{ accept: %{ any: true }, reverse_host: ["vendor-management" | _rest] }  do
    forward conn, path, "http://vendor-management/assets/"
  end

  get "/@appuniversum/*path", %{ accept: %{ any: true }, reverse_host: ["vendor-management" | _rest] } do
    forward conn, path, "http://vendor-management/@appuniversum/"
  end

  match "/*_path", %{ accept: %{ html: true }, reverse_host: ["vendor-management" | _rest] } do
    forward conn, [], "http://vendor-management/index.html"
  end

  #################################################################
  # Loket
  #################################################################

  # NOTE: keep this as the last frontend. There is no host/reverse_host
  # matching. This catches all attempts to access a frontend and should,
  # because of the order sensitivity of mu-auth, come last.
  # Some loket instances are hosted like "dev.loket.[...]" which make matching
  # difficult.

  # Login

  match "/sessions/*path" do
    forward conn, path, "http://login-loket/sessions/"
  end

  # Frontend

  get "/assets/*path", @any do
    forward conn, path, "http://loket/assets/"
  end

  get "/@appuniversum/*path", @any do
    forward conn, path, "http://loket/@appuniversum/"
  end

  match "/*_path", @html do
    forward conn, [], "http://loket/index.html"
  end

  #################################################################
  # Other
  #################################################################

  match "/*_" do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
