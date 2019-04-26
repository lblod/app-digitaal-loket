alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  defp access_by_role( group_string ) do
    %AccessByQuery{
      vars: ["session_group","session_role"],
      query: sparql_query_for_access_role( group_string ) }
  end

  defp sparql_query_for_access_role( group_string ) do
    "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    SELECT ?session_group ?session_role WHERE {
      <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                   ext:sessionRole ?session_role.
      FILTER( ?session_role = \"#{group_string}\" )
    }"
  end

  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      # // PUBLIC
      %GroupSpec{
        name: "public",
        useage: [:read],
        access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/validation/Execution",
                        "http://mu.semte.ch/vocabularies/validation/Validation",
                        "http://mu.semte.ch/vocabularies/validation/Error",
                        "http://mu.semte.ch/vocabularies/ext/FormNode",
                        "http://mu.semte.ch/vocabularies/ext/FormInput",
                        "http://mu.semte.ch/vocabularies/ext/DynamicSubform",
                        "http://mu.semte.ch/vocabularies/ext/DocumentStatus",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
                        "http://mu.semte.ch/vocabularies/ext/supervision/InzendingType",
                        "http://mu.semte.ch/vocabularies/ext/supervision/DecisionType",
                        "http://mu.semte.ch/vocabularies/ext/supervision/TaxType",
                        "http://mu.semte.ch/vocabularies/ext/supervision/Nomenclature",
                        "http://mu.semte.ch/vocabularies/ext/supervision/FiscalPeriod",
                        "http://mu.semte.ch/vocabularies/ext/supervision/DeliveryReportType",
                        "http://mu.semte.ch/vocabularies/ext/supervision/AccountAcceptanceStatus",
                        "http://mu.semte.ch/vocabularies/ext/supervision/DocumentAuthenticityType",
                        "http://mu.semte.ch/vocabularies/ext/supervision/RegulationType",
                        "http://www.w3.org/ns/prov#Location",
                        "http://mu.semte.ch/vocabularies/ext/BestuurseenheidClassificatieCode",
                        "http://data.vlaanderen.be/ns/besluit#Bestuursorgaan",
                        "http://mu.semte.ch/vocabularies/ext/BestuursorgaanClassificatieCode",
                        "http://mu.semte.ch/vocabularies/ext/Fractietype",
                        "http://mu.semte.ch/vocabularies/ext/KandidatenlijstType",
                        "http://data.vlaanderen.be/ns/mandaat#Kandidatenlijst",
                        "http://data.vlaanderen.be/ns/mandaat#Mandaat",
                        "http://mu.semte.ch/vocabularies/ext/BestuursfunctieCode",
                        "http://mu.semte.ch/vocabularies/ext/MandatarisStatusCode",
                        "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
                        "http://mu.semte.ch/vocabularies/ext/GeslachtCode",
                        "http://data.europa.eu/eli/ontology#LegalResource",
                        "http://data.vlaanderen.be/ns/mandaat#RechtsgrondAanstelling",
                        "http://data.vlaanderen.be/ns/mandaat#RechtsgrondBeeindiging",
                        "http://data.vlaanderen.be/ns/mandaat#RechtstreekseVerkiezing",
                        "http://data.vlaanderen.be/ns/mandaat#Verkiezingsresultaat",
                        "http://mu.semte.ch/vocabularies/ext/VerkiezingsresultaatGevolgCode",
                        "http://www.w3.org/ns/org#Role",
                        "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
                        "http://data.lblod.info/vocabularies/lblod/FunctionarisStatusCode",
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#Entry",
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#Level",
                        "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#StatusCode",
                        "http://mu.semte.ch/vocabularies/ext/LogSource"
                      ]
                    } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/sessions",
                    constraint: %ResourceFormatConstraint{
                      resource_prefix: "http://mu.semte.ch/sessions/"
                    } } ] },
      %GroupSpec{
        name: "public-wf",
        useage: [:write, :read_for_write],
        access: %AlwaysAccessible{}, # TODO: Should be only for logged in users
        graphs: [%GraphSpec{
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/ext/BeleidsdomeinCode",
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject"
                      ]
                    } } ] },
      # // ORGANIZATION HAS POSSIBLY DUPLICATE USER DATA
      %GroupSpec{
        name: "org",
        useage: [:read],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT ?session_group ?session_role WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group.
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://xmlns.com/foaf/0.1/Person",
                        "http://xmlns.com/foaf/0.1/OnlineAccount",
                        "http://www.w3.org/ns/adms#Identifier"
                      ] } } ] },

      # // ORG-MANDATENBEHEER
      %GroupSpec{
        name: "o-mdb-r",
        useage: [:read],
        access: access_by_role( "LoketLB-mandaatGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/org#Membership",
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://www.w3.org/ns/person#Person",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://purl.org/dc/terms/PeriodOfTime",
                        "http://www.w3.org/ns/org#Site",
                        "http://schema.org/PostalAddress",
                        "http://www.w3.org/ns/org#Post",
                        "http://www.w3.org/ns/org#Organization" ] } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [ "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid" ],
                      predicates: %NoPredicates{
                        except: [
                          "http://www.w3.org/ns/org#hasPrimarySite",
                          "http://www.w3.org/ns/org#linkedTo",
                          "http://schema.org/contactPoint",
                          "http://www.w3.org/ns/org#hasPost" ] } } } ] },
      %GroupSpec{
        name: "o-mdb-wf",
        useage: [:write, :read_for_write],
        access: access_by_role( "LoketLB-mandaatGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://data.vlaanderen.be/ns/mandaat#Fractie",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/org#Membership",
                        "http://data.vlaanderen.be/ns/mandaat#Mandataris",
                        "http://www.w3.org/ns/person#Person",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://purl.org/dc/terms/PeriodOfTime",
                        "http://www.w3.org/ns/org#Site",
                        "http://schema.org/PostalAddress",
                        "http://www.w3.org/ns/org#Post",
                        "http://www.w3.org/ns/org#Organization" ] } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [ "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid" ],
                      predicates: %NoPredicates{
                        except: [
                          "http://www.w3.org/ns/org#hasPrimarySite",
                          "http://www.w3.org/ns/org#linkedTo",
                          "http://schema.org/contactPoint",
                          "http://www.w3.org/ns/org#hasPost" ] } } } ] },

      # // BBCDR
      %GroupSpec{
        name: "o-bbcdr-rwf",
        useage: [:read, :write, :read_for_write],
        access: access_by_role( "LoketLB-bbcdrGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/ext/bbcdr/Report"
                      ] } } ] },

      # // TOEZICHT
      %GroupSpec{
        name: "o-toez-rwf",
        useage: [:read, :write, :read_for_write],
        access: access_by_role( "LoketLB-toezichtGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://mu.semte.ch/vocabularies/ext/supervision/InzendingVoorToezicht",
                        "http://mu.semte.ch/vocabularies/ext/supervision/TaxRate",
                        "http://mu.semte.ch/vocabularies/ext/FormSolution",
                        "http://mu.semte.ch/vocabularies/ext/FileAddress"
                      ] } } ] },

      # // LEIDINGGEVENDE
      %GroupSpec{
        name: "o-leidinggevende-rwf",
        useage: [:read, :write, :read_for_write],
        access: access_by_role( "LoketLB-leidinggevendenGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://www.w3.org/ns/org#Site",
                        "http://schema.org/PostalAddress",
                        "http://www.w3.org/ns/org#Post",
                        "http://www.w3.org/ns/org#Organization",
                        "http://data.lblod.info/vocabularies/lblod/Functionaris",
                        "http://data.vlaanderen.be/ns/persoon#Geboorte",
                        "http://www.w3.org/ns/person#Person",
                        "http://www.w3.org/ns/adms#Identifier",
                        "http://purl.org/dc/terms/PeriodOfTime"
                      ] } } ] },

      # // MESSAGING CENTRE
      %GroupSpec{
        name: "o-messaging-rwf",
        useage: [:read, :write, :read_for_write],
        access: access_by_role( "LoketLB-berichtenGebruiker" ),
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [
                        "http://schema.org/Message",
                        "http://schema.org/Conversation"
                      ] } },
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: [ "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid" ],
                      predicates: %NoPredicates{
                        except: [
                          "http://mu.semte.ch/vocabularies/ext/mailAdresVoorNotificaties",
                          "http://mu.semte.ch/vocabularies/ext/wilMailOntvangen"
                        ] }
                    } } ] },



      # // USER HAS NO DATA
      # this was moved to org instead.
      # perhaps move some elements to public when needed for demo
      # purposes.


      # // CLEANUP
      #
      %GraphCleanup{
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end
