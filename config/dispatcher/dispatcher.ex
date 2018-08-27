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
  match "/entiteiten/*path" do
    Proxy.forward conn, path, "http://cache/entiteiten/"
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
  match "/vestigingen/*path" do
    Proxy.forward conn, path, "http://cache/vestigingen/"
  end
  match "/contact-punten/*path" do
    Proxy.forward conn, path, "http://cache/contact-punten/"
  end
  match "/posities/*path" do
    Proxy.forward conn, path, "http://cache/posities/"
  end
  match "/rollen/*path" do
    Proxy.forward conn, path, "http://cache/rollen/"
  end
  match "/tijdsintervallen/*path" do
    Proxy.forward conn, path, "http://cache/tijdsintervallen/"
  end
  match "/organisaties/*path" do
    Proxy.forward conn, path, "http://cache/organisaties/"
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
  post "/file-service/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end
  delete "/files/*path" do
    Proxy.forward conn, path, "http://file/files/"
  end
  post "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://createreports/bbcdr-reports/"
  end
  delete "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end
  get "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://resource/bbcdr-reports/"
  end
  patch "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://createreports/bbcdr-reports/"
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
  # master-toezicht-domain.lisp
  ###############################################################
  get "/toezicht/bestanden/*path" do
    Proxy.forward conn, path, "http://filehost/"
  end

  match "/inzendingen-voor-toezicht/*path" do
    Proxy.forward conn, path, "http://cache/inzendingen-voor-toezicht/"
  end
  match "/toezicht-tax-types/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-tax-types/"
  end
  match "/toezicht-nomenclatures/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-nomenclatures/"
  end
  match "/toezicht-fiscal-periods/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-fiscal-periods/"
  end
  match "/toezicht-delivery-report-types/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-delivery-report-types/"
  end
  match "/toezicht-account-acceptance-statuses/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-account-acceptance-statuses/"
  end
  match "/toezicht-document-authenticity-types/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-document-authenticity-types/"
  end
  match "/toezicht-inzending-types/*path" do
    Proxy.forward conn, path, "http://cache/toezicht-inzending-types/"
  end
  match "/besluit-types/*path" do
    Proxy.forward conn, path, "http://cache/besluit-types/"
  end
  match "/tax-rates/*path" do
    Proxy.forward conn, path, "http://cache/tax-rates/"
  end
  match "/form-solutions/*path" do
    Proxy.forward conn, path, "http://cache/form-solutions/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
