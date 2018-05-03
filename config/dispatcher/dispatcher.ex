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
    Proxy.forward conn, path, "http://resource/mandatarissen/"
  end
  match "/mandataris-status-codes/*path" do
    Proxy.forward conn, path, "http://cache/mandataris-status-codes/"
  end
  match "/beleidsdomein-codes/*path" do
    Proxy.forward conn, path, "http://resource/beleidsdomein-codes/"
  end
  match "/personen/*path" do
    Proxy.forward conn, path, "http://resource/personen/"
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
  match "/files/*path" do
    Proxy.forward conn, path, "http://resource/files/"
  end
  match "/file-service/*path" do
    Proxy.forward conn, path, "http://file-service/"
  end
  match "/bbcdr-reports/*path" do
    Proxy.forward conn, path, "http://cache/bbcdr-reports/"
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

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
