;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(add-delta-logger)
(add-delta-messenger "http://deltanotifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *backend* "http://virtuoso:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil)

;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  :adms "http://www.w3.org/ns/adms#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :cal "http://www.w3.org/2002/12/cal/ical#"
  :ch "http://data.lblod.info/vocabularies/contacthub/"
  :cogs "http://vocab.deri.ie/cogs#"
  :dcat "http://www.w3.org/ns/dcat#"
  :dct "http://purl.org/dc/terms/"
  :eli "http://data.europa.eu/eli/ontology#"
  :employee "http://lblod.data.gift/vocabularies/employee/"
  :ered "http://data.lblod.info/vocabularies/erediensten/"
  :euvoc "http://publications.europa.eu/ontology/euvoc#"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :foaf "http://xmlns.com/foaf/0.1/"
  :generiek "https://data.vlaanderen.be/ns/generiek#"
  :leidinggevenden "http://data.lblod.info/vocabularies/leidinggevenden/"
  :locn "http://www.w3.org/ns/locn#"
  :m8g "http://data.europa.eu/m8g/"
  :mandaat "http://data.vlaanderen.be/ns/mandaat#"
  :mu "http://mu.semte.ch/vocabularies/core/"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :org "http://www.w3.org/ns/org#"
  :organisatie "http://lblod.data.gift/vocabularies/organisatie/"
  :oslc "http://open-services.net/ns/core#"
  :person "http://www.w3.org/ns/person#"
  :persoon "http://data.vlaanderen.be/ns/persoon#"
  :prov "http://www.w3.org/ns/prov#"
  :qudt "http://qudt.org/schema/qudt/"
  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
  :rlog "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#"
  :schema "http://schema.org/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :tempo "http://purl.org/tempo/"
  :validation "http://mu.semte.ch/vocabularies/validation/"
  :wf "http://www.w3.org/2005/01/wf/flow#"
  :xsd "http://www.w3.org/2001/XMLSchema#")

(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")

(define-graph sessions ("http://mu.semte.ch/graphs/sessions")
  ("http://mu.semte.ch/vocabularies/session/Session" -> _))

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("validation:Execution" -> _)
  ("validation:Validation" -> _)
  ("validation:Error" -> _)
  ("ext:FormNode" -> _)
  ("ext:FormInput" -> _)
  ("ext:DynamicSubform" -> _)
  ("ext:DocumentStatus" -> _)
  ("nfo:FileDataObject" -> _)
  ("ext:supervision/InzendingType" -> _)
  ("ext:supervision/DecisionType" -> _)
  ("ext:supervision/TaxType" -> _)
  ("ext:supervision/Nomenclature" -> _)
  ("ext:supervision/FiscalPeriod" -> _)
  ("ext:supervision/DeliveryReportType" -> _)
  ("ext:supervision/AccountAcceptanceStatus" -> _)
  ("ext:supervision/DocumentAuthenticityType" -> _)
  ("ext:supervision/RegulationType" -> _)
  ("prov:Location" -> _)
  ("ext:BestuurseenheidClassificatieCode" -> _)
  ("besluit:Bestuursorgaan" -> _)
  ("ext:BestuursorgaanClassificatieCode" -> _)
  ("ext:Fractietype" -> _)
  ("ext:KandidatenlijstType" -> _)
  ("mandaat:Kandidatenlijst" -> _)
  ("mandaat:Mandaat" -> _)
  ("ext:BestuursfunctieCode" -> _)
  ("ext:MandatarisStatusCode" -> _)
  ("ext:BeleidsdomeinCode" -> _)
  ("ext:GeslachtCode" -> _)
  ("euvoc:Country" -> _)
  ("eli:LegalResource" -> _)
  ("mandaat:RechtsgrondAanstelling" -> _)
  ("mandaat:RechtsgrondBeeindiging" -> _)
  ("mandaat:RechtstreekseVerkiezing" -> _)
  ("mandaat:Verkiezingsresultaat" -> _)
  ("ext:VerkiezingsresultaatGevolgCode" -> _)
  ("org:Role" -> _)
  ("besluit:Bestuurseenheid" -> _)
  ("leidinggevenden:FunctionarisStatusCode" -> _)
  ("leidinggevenden:Bestuursfunctie" -> _)
  ("rlog:Entry" -> _)
  ("rlog:Level" -> _)
  ("rlog:StatusCode" -> _)
  ("ext:LogSource" -> _)
  ("employee:EmployeeTimePeriod" -> _)
  ("employee:UnitMeasure" -> _)
  ("employee:EducationalLevel" -> _)
  ("employee:WorkingTimeCategory" -> _)
  ("employee:LegalStatus" -> _)
  ("ext:ChartOfAccount" -> _)
  ("ext:AuthenticityType" -> _)
  ("ext:TaxType" -> _)
  ("ext:SubmissionDocumentStatus" -> _)
  ("besluit:Zitting" -> _)
  ("besluit:Agendapunt" -> _)
  ("besluit:BehandelingVanAgendapunt" -> _)
  ("skos:ConceptScheme" -> _)
  ("skos:Concept" -> _)
  ("m8g:PeriodOfTime" -> _)
  ("m8g:Criterion" -> _)
  ("m8g:RequirementGroup" -> _)
  ("m8g:CriterionRequirement" -> _)
  ("m8g:Requirement" -> _)
  ("foaf:Document" -> _)
  ("org:Organization" -> _)
  ("organisatie:EredienstBeroepen" -> _)
  ("organisatie:BedienaarFinancieringCode" -> _)
  ("organisatie:VoorwaardenBedienaarCriterium" -> _)
  ("organisatie:BedienaarCriteriumBewijsstuk" -> _)
  ("organisatie:TypeVestiging" -> _)
  ("organisatie:OrganisatieStatusCode" -> _)
  ("organisatie:TypeBetrokkenheid" -> _)
  ("organisatie:TypeEredienst" -> _)
  ("organisatie:HelftVerkiezing" -> _))

(define-graph public-r ("http://mu.semte.ch/graphs/authenticated/public")
  ("besluit:Bestuurseenheid" -> "ext:viewOnlyModules"))

(define-graph org ("http://mu.semte.ch/graphs/organizations/")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
  ("adms:Identifier" -> _))

(define-graph o-bbcdr-rw ("http://mu.semte.ch/graphs/organizations/")
  ("ext:bbcdr/Report" -> _)
  ("nfo:FileDataObject" -> _))

;; Toezicht
(define-graph o-toez-rw ("http://mu.semte.ch/graphs/organizations/")
  ("cogs:Job" -> _)
  ("ext:supervision/InzendingVoorToezicht" -> _)
  ("ext:supervision/TaxRate" -> _)
  ("ext:supervision/SimplifiedTaxRate" -> _)
  ("ext:FormSolution" -> _)
  ("ext:FileAddress" -> _)
  ("foaf:Document" -> _)
  ("http://rdf.myexperiment.org/ontologies/base/Submission" -> _)
  ("ext:SubmissionDocument" -> _)
  ("http://lblod.data.gift/vocabularies/besluit/TaxRate" -> _)
  ("http://lblod.data.gift/vocabularies/automatische-melding/FormData" -> _)
  ("nfo:FileDataObject" -> _)
  ("nfo:DataContainer" -> _)
  ("nfo:LocalFileDataObject" -> _)
  ("nfo:RemoteDataObject" -> _)
  ("http://lblod.data.gift/services/Service" -> _)
  ("http://lblod.data.gift/vocabularies/harvesting/HarvestingCollection" -> _)
  ("http://redpencil.data.gift/vocabularies/tasks/Operation" -> _)
  ("http://redpencil.data.gift/vocabularies/tasks/Task" -> _)
  ("cogs:ExecutionStatus" -> _))

;; Vendor Management
(define-graph o-toezicht-vendor-management-rw ("http://mu.semte.ch/graphs/automatic-submission")
  ("ext:Vendor" -> _)
  ("besluit:Bestuurseenheid" -> _))

(define-graph o-toezicht-vendor-management-authenticated-rw ("http://mu.semte.ch/graphs/authenticated/public")
  ("besluit:Bestuurseenheid" -> "ext:viewOnlyModules"))

;; LeidingGevenden
(define-graph o-leidinggevenden-rw ("http://mu.semte.ch/graphs/organizations/")
  ("http://data.lblod.info/vocabularies/contacthub/AgentInPositie" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("http://data.lblod.info/vocabularies/leidinggevenden/Functionaris" -> _)
  ("persoon:Geboorte" -> _)
  ("person:Person" -> _)
  ("adms:Identifier" -> _)
  ("leidinggevenden:Bestuursfunctie" -> "schema:contactPoint"))


;; Messaging Centre
(define-graph o-messaging-rw ("http://mu.semte.ch/graphs/organizations/")
  ("schema:Message" -> _)
  ("schema:Conversation" -> _)
  ("nfo:FileDataObject" -> _)
  ("besluit:Bestuurseenheid" -> "ext:mailAdresVoorNotificaties")
  ("besluit:Bestuurseenheid" -> "ext:wilMailOntvangen"))


;; Employee Numbers Database
(define-graph o-employee-database-rw ("http://mu.semte.ch/graphs/organizations/")
  ("employee:EmployeeDataset" -> _)
  ("employee:EmployeePeriodSlice" -> _)
  ("employee:EmployeeObservation" -> _))


;; Worship Mandatees
(define-graph o-worship-positions-rw ("http://mu.semte.ch/graphs/organizations/")
  ("ch:AgentInPositie" -> _)
  ("adms:Identifier" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("mandaat:Mandataris" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("org:Post" -> _)
  ("org:Site" -> _)
  ("ered:EredienstMandataris" -> _)
  ("ered:EredienstBestuurseenheid" -> _)
  ("ered:BestuurVanDeEredienst" -> _)
  ("ered:CentraalBestuurVanDeEredienst" -> _)
  ("ered:RepresentatiefOrgaan" -> _)
  ("ered:BetrokkenLokaleBesturen" -> _)
  ("generiek:GestructureerdeIdentificator" -> _)
  ("organisatie:HelftVerkiezing" -> _))


(define-graph o-worship-positions-bedienaar-rw ("http://mu.semte.ch/graphs/organizations/")
  ("adms:Identifier" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("mandaat:Mandataris" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("ch:AgentInPositie" -> _)
  ("org:Post" -> _)
  ("org:Site" -> _)
  ("ered:EredienstBestuurseenheid" -> _)
  ("ered:BestuurVanDeEredienst" -> _)
  ("ered:CentraalBestuurVanDeEredienst" -> _)
  ("ered:RepresentatiefOrgaan" -> _)
  ("ered:BetrokkenLokaleBesturen" -> _)
  ("ered:RolBedienaar" -> _)
  ("ered:VoorwaardenBedienaar" -> _)
  ("ered:PositieBedienaar" -> _)
  ("generiek:GestructureerdeIdentificator" -> _))


;; Toezicht Vendor API
(define-graph o-vendor-api-r ("http://mu.semte.ch/graphs/vendors/")
  ("http://rdf.myexperiment.org/ontologies/base/Submission" -> _)
  ("ext:SubmissionDocument" -> _)
  ("http://lblod.data.gift/vocabularies/automatische-melding/FormData" -> _)
  ("schema:Conversation" -> _)
  ("schema:Message" -> _)
  ("cogs:Job" -> _)
  ("nfo:FileDataObject" -> _))


(define-graph o-admin-rw ("http://mu.semte.ch/graphs/organizations/")
  ("http://lblod.data.gift/vocabularies/reporting/Report" -> _)
  ("cogs:Job" -> _)
  ("oslc:Error" -> _)
  ("nfo:DataContainer" -> _)
  ("nfo:FileDataObject" -> _)
  ("nfo:DataContainer" -> _))

;; LOKETADMIN -> TODO: duplicate. We need to move the data in this graph to "http://mu.semte.ch/graphs/organizations/"
(define-graph o-admin-harvesting-rw ("http://mu.semte.ch/graphs/harvesting")
  ("http://lblod.data.gift/vocabularies/reporting/Report" -> _)
  ("cogs:Job" -> _)
  ("oslc:Error" -> _)
  ("nfo:DataContainer" -> _)
  ("nfo:FileDataObject" -> _)
  ("nfo:DataContainer" -> _))

(define-graph o-persons-sensitive-deltas-r ("http://redpencil.data.gift/id/deltas/producer/")
  ("nfo:FileDataObject" -> _)
  ("dcat:Dataset" -> _)
  ("dcat:Distribution" -> _))

(supply-allowed-group "public")

(supply-allowed-group "authenticated"
  :parameters ()
  :query  "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        SELECT DISTINCT ?session_group ?session_role WHERE {
          <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                       ext:sessionRole ?session_role.
        }"
  )

(supply-allowed-group "access-automatic-submission"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT DISTINCT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                    ext:sessionRole ?session_role.
      FILTER( ?session_role = \"VMTGebruiker\" )
    }")

(supply-allowed-group "access-sensitive-delta-producer-data"
  :parameters ("group_name")
  :query "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
    SELECT DISTINCT ?group_name WHERE {

      VALUES ?group_name {
        \"persons-sensitive-deltas\"
        \"subsidies-deltas\"
        \"worship-submissions-deltas\"
        \"worship-services-sensitive-deltas\"
        \"vendor-management-deltas\"
      }

      <SESSION_ID> muAccount:account ?onlineAccount.

      ?onlineAccount  a foaf:OnlineAccount.

      ?agent a foaf:Agent;
        foaf:account ?onlineAccount.

      ?group foaf:member ?agent;
        foaf:name ?group_name.
    }")

(supply-allowed-group "access-for-vendor-api"
  :parameters ("vendor_id" "session_group")
  :query "PREFIX muAccount: <http://mu.semte.ch/vocabularies/account/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT DISTINCT ?vendor_id ?session_group WHERE {
      <SESSION_ID> muAccount:canActOnBehalfOf/mu:uuid ?session_group;
                   muAccount:account/mu:uuid ?vendor_id.
    }")

(supply-allowed-group "admin"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

    SELECT DISTINCT ?session_role WHERE {
      VALUES ?session_role {
        \"LoketLB-admin\"
      }

      VALUES ?session_id {
        <SESSION_ID>
      }

      {
        ?session_id ext:sessionRole ?session_role .
      } UNION {
        ?session_id ext:originalSessionRole ?session_role .
      }
    } LIMIT 1")


(supply-allowed-group "loket-admin"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT DISTINCT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role = \"LoketAdmin\" )
    }")

(supply-allowed-group "logged-in-or-impersonating"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT DISTINCT ?session_group WHERE {
      {
        <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
      } UNION {
        <SESSION_ID> ext:originalSessionGroup/mu:uuid ?session_group.
      }
    }")

(dolist (role (list "LoketLB-bbcdrGebruiker"
                    "LoketLB-toezichtGebruiker"
                    "LoketLB-leidinggevendenGebruiker"
                    "LoketLB-berichtenGebruiker"
                    "LoketLB-personeelsbeheer"
                    "LoketLB-eredienstMandaatGebruiker"
                    "LoketLB-eredienstBedienaarGebruiker"
                    "LoketAdmin"))

  (eval
   `(supply-allowed-group ,role
      :parameters ("session_group" "session_role")
      :query ,(format nil "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT DISTINCT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                    ext:sessionRole ?session_role.
      FILTER( ?session_role = \"~a\" )
    }" role))))

(grant (read)
  :to-graph (public sessions)
  :for-allowed-group "public")

(grant (read)
  :to-graph (public-r)
  :for-allowed-group "authenticated")

(grant (read)
  :to-graph (org)
  :for-allowed-group "logged-in-or-impersonating")

(grant (read write)
  :to-graph (o-bbcdr-rw)
  :for-allowed-group "LoketLB-bbcdrGebruiker")

(grant (read write)
  :to-graph (o-toez-rw)
  :for-allowed-group "LoketLB-toezichtGebruiker")

(grant (read write)
  :to-graph (o-toezicht-vendor-management-rw)
  :for-allowed-group "access-automatic-submission")

(grant (read write)
  :to-graph (o-toezicht-vendor-management-authenticated-rw)
  :for-allowed-group "access-automatic-submission")

(grant (read write)
  :to-graph (o-leidinggevenden-rw)
  :for-allowed-group "LoketLB-leidinggevendenGebruiker")

(grant (read write)
  :to-graph (o-messaging-rw)
  :for-allowed-group "LoketLB-berichtenGebruiker")

(grant (read write)
  :to-graph (o-employee-database-rw)
  :for-allowed-group "LoketLB-personeelsbeheer")

(grant (read write)
  :to-graph (o-worship-positions-rw)
  :for-allowed-group "LoketLB-eredienstMandaatGebruiker")

(grant (read write)
  :to-graph (o-worship-positions-bedienaar-rw)
  :for-allowed-group "LoketLB-eredienstBedienaarGebruiker")

(grant (read)
  :to-graph (o-vendor-api-r)
  :for-allowed-group "access-for-vendor-api")

(grant (read write)
  :to-graph (sessions)
  :for-allowed-group "admin")

(grant (read write)
  :to-graph (o-admin-rw)
  :for-allowed-group "LoketAdmin")

(grant (read write)
  :to-graph (o-admin-harvesting-rw)
  :for-allowed-group "loket-admin")

(grant (read)
  :to-graph (o-persons-sensitive-deltas-r)
  :for-allowed-group "access-sensitive-delta-producer-data")
