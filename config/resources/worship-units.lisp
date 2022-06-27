(define-resource agent-in-position ()
  :class (s-prefix "ch:AgentInPositie")
  :properties `((:agent-start-date :date ,(s-prefix "ch:startdatum"))
                (:agent-end-date :date ,(s-prefix "ch:eindedatum")))
  :has-one `((post :via ,(s-prefix "org:holds")
                   :as "position")
             (persoon :via ,(s-prefix "org:heldBy")
                      :as "person"))
  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contacts"))
  :resource-base (s-url "http://data.lblod.info/id/agentenInPositie/")
  :features '(include-uri)
  :on-path "agents-in-position"
)

(define-resource post ()
  :class (s-prefix "org:Post")
  :has-one `((role :via ,(s-prefix "org:role")
                   :as "general-role")
             (organization :via ,(s-prefix "org:postIn")
                            :as "organization"))
  :has-many `((agent-in-position :via ,(s-prefix "org:holds")
                                 :inverse t
                                 :as "agents-in-position"))
  :resource-base (s-url "http://data.lblod.info/id/posities/")
  :features '(include-uri)
  :on-path "posts"
)

(define-resource worship-mandatee (mandataris)
  :class (s-prefix "ere:EredienstMandataris")
  :properties `((:expected-end-date :date ,(s-prefix "ere:geplandeEinddatumAanstelling"))
                (:reason-stopped :string ,(s-prefix "ere:redenVanStopzetting")))
  :has-one `((half-election :via ,(s-prefix "ere:typeHelft")
                            :as "type-half"))
  :resource-base (s-url "http://data.lblod.info/id/mandatarissen/")
  :features '(include-uri)
  :on-path "worship-mandatees"
)

(define-resource half-election ()
  :class (s-prefix "code:HelftVerkiezing")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "half-elections"
)

(define-resource role ()
  :class (s-prefix "org:Role")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/rollen/")
  :features '(include-uri)
  :on-path "roles"
)

(define-resource organization ()
  :class (s-prefix "org:Organization")
  :properties `((:name :string ,(s-prefix "skos:prefLabel"))
                (:alternative-name :string ,(s-prefix "skos:altLabel")))
  :has-one `((organization-status-code :via ,(s-prefix "regorg:orgStatus")
                                       :as "organization-status")
             (site :via ,(s-prefix "org:hasPrimarySite")
                   :as "primary-site")
             (organization :via ,(s-prefix "org:linkedTo")
                           :inverse t
                           :as "is-associated-with")
             (organization :via ,(s-prefix "org:hasSubOrganization")
                           :inverse t
                           :as "is-sub-organization-of"))
  :has-many `((identificator :via ,(s-prefix "adms:identifier")
                             :as "identifiers")
              (site :via ,(s-prefix "org:hasSite")
                    :as "sites")
              (change-event :via ,(s-prefix "org:originalOrganization")
                            :inverse t
                            :as "changed-by")
              (change-event :via ,(s-prefix "org:resultingOrganization")
                            :inverse t
                            :as "resulted-from")
              (change-event-result :via ,(s-prefix "ext:resultingOrganization")
                                   :inverse t
                                   :as "change-event-results")
              (post :via ,(s-prefix "org:hasPost")
                    :as "positions")
              (organization :via ,(s-prefix "org:linkedTo")
                            :as "associated-organizations")
              (organization :via ,(s-prefix "org:hasSubOrganization")
                            :as "sub-organizations"))
  :resource-base (s-url "http://data.lblod.info/id/organisaties/")
  :features '(include-uri)
  :on-path "organizations"
)

(define-resource worship-administrative-unit ()
  :class (s-prefix "org:Post")
  :has-one `((recognized-worship-type :via ,(s-prefix "ere:typeEredienst")
                                      :as "recognized-worship-type"))
  :has-many `((minister-position :via ,(s-prefix "ere:wordtBediendDoor")
                                 :inverse t
                                 :as "minister-positions"))
  :resource-base (s-url "http://data.lblod.info/id/eredienstBestuurseenheden/")
  :features '(include-uri)
  :on-path "worship-administrative-units"
)

(define-resource worship-service (worship-administrative-unit)
  :class (s-prefix "ere:BestuurVanDeEredienst")
  :properties `((:denomination :string ,(s-prefix "ere:denominatie"))
                (:cross-border :boolean ,(s-prefix "ere:grensoverschrijdend")))
  :has-one `((involvement :via ,(s-prefix "org:organization")
                          :as "local-involvement"))
  :resource-base (s-url "http://data.lblod.info/id/besturenVanDeEredienst/")
  :features '(include-uri)
  :on-path "worship-services"
)

(define-resource recognized-worship-type ()
  :class (s-prefix "code:TypeEredienst")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "recognized-worship-types"
)

(define-resource central-worship-service (worship-administrative-unit)
  :class (s-prefix "ere:CentraalBestuurVanDeEredienst")
  :resource-base (s-url "http://data.lblod.info/id/centraleBesturenVanDeEredienst/")
  :features '(include-uri)
  :on-path "central-worship-services"
)

(define-resource representative-body ()
  :class (s-prefix "ere:RepresentatiefOrgaan")
  :has-one `((recognized-worship-type :via ,(s-prefix "ere:typeEredienst")
                                      :as "recognized-worship-type"))
  :has-many `((minister-position :via ,(s-prefix "ere:behoort")
                                 :inverse t
                                 :as "minister-positions"))
  :resource-base (s-url "http://data.lblod.info/id/representatieveOrganen/")
  :features '(include-uri)
  :on-path "representative-bodies"
)

(define-resource local-involvement ()
  :class (s-prefix "ere:BetrokkenLokaleBesturen")
  :properties `((:percentage :number ,(s-prefix "ere:financieringspercentage")))
  :has-one `((involvement-type :via ,(s-prefix "ere:typebetrokkenheid")
                               :as "involvement-type")
             (worship-service :via ,(s-prefix "org:organization")
                              :as "worship-service")
             (bestuurseenheid :via ,(s-prefix "ere:betrokkenBestuur")
                              :inverse t
                              :as "administrative-unit"))
  :resource-base (s-url "http://data.lblod.info/id/betrokkenLokaleBesturen/")
  :features '(include-uri)
  :on-path "local-involvements"
)

(define-resource structured-identifier ()
  :class (s-prefix "generiek:GestructureerdeIdentificator")
  :properties `((:local-id :string ,(s-prefix "generiek:lokaleIdentificator")))
  :resource-base (s-url "http://data.lblod.info/id/gestructureerdeIdentificatoren/")
  :features '(include-uri)
  :on-path "structured-identifiers"
)

(define-resource site ()
  :class (s-prefix "org:Site")
  :has-one `((adres :via ,(s-prefix "organisatie:bestaatUit")
                    :as "address")
             (site-type :via ,(s-prefix "ere:vestigingstype")
                        :as "site-type"))
  :has-many `((contact-punt :via ,(s-prefix "org:siteAddress")
                            :as "contacts"))
  :resource-base (s-url "http://data.lblod.info/id/vestigingen/")
  :features '(include-uri)
  :on-path "sites"
)

(define-resource organization-status-code ()
  :class (s-prefix "code:OrganisatieStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "organization-status-codes"
)

(define-resource involvement-type ()
  :class (s-prefix "code:TypeBetrokkenheid")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "involvement-types"
)

(define-resource minister (agent-in-position)
  :class (s-prefix "ere:RolBedienaar")
  :has-one `((minister-position :via ,(s-prefix "org:holds")
                                :as "minister-position")
             (financing-code :via ,(s-prefix "ere:financiering")
                             :as "financing"))
  :has-many `((minister-condition :via ,(s-prefix "org:siteAddress")
                                  :as "conditions"))
  :resource-base (s-url "http://data.lblod.info/id/rollenBedienaar/")
  :features '(include-uri)
  :on-path "ministers"
)

(define-resource minister-condition ()
  :class (s-prefix "ere:VoorwaardenBedienaar")
  :properties `((:satisfied :boolean ,(s-prefix "ere:voldaan")))
  :has-one `((minister-condition-criterion :via ,(s-prefix "ere:criterium")
                                           :as "criterion")
             (document-type-criterion :via ,(s-prefix "ere:criteriumbewijsstuktype")
                                      :as "document-type-criterion"))
  :resource-base (s-url "http://data.lblod.info/id/voorwaardenBedienaar/")
  :features '(include-uri)
  :on-path "minister-conditions"
)

(define-resource minister-position (post)
  :class (s-prefix "ere:PositieBedienaar")
  :has-one `((minister-position-function :via ,(s-prefix "ere:functie")
                                         :as "function")
             (worship-administrative-unit :via ,(s-prefix "ere:wordtBediendDoor")
                                          :inverse t
                                          :as "worship-service")
             (representative-body :via ,(s-prefix "ere:behoort")
                                          :as "representative-body"))
  :has-many `((minister :via ,(s-prefix "org:holds")
                        :inverse t
                        :as "held-by-ministers"))
  :resource-base (s-url "http://data.lblod.info/id/positiesBedienaar/")
  :features '(include-uri)
  :on-path "minister-positions"
)

(define-resource minister-position-function ()
  :class (s-prefix "code:EredienstBeroepen")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "minister-position-functions"
)

(define-resource financing-code ()
  :class (s-prefix "code:BedienaarFinancieringCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "financing-codes"
)

(define-resource minister-condition-criterion ()
  :class (s-prefix "code:VoorwaardenBedienaarCriterium")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "minister-condition-criterions"
)

(define-resource document-types-criterion ()
  :class (s-prefix "code:BedienaarCriteriumBewijsstuk")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "document-types-criterions"
)

(define-resource site-type ()
  :class (s-prefix "code:TypeVestiging")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features '(include-uri)
  :on-path "site-types"
)