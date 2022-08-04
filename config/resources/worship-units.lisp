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

(define-resource worship-service ()
  :class (s-prefix "ere:BestuurVanDeEredienst")
  :properties `((:denomination :string ,(s-prefix "ere:denominatie"))
                (:cross-border :boolean ,(s-prefix "ere:grensoverschrijdend")))
  :has-one `((local-involvement :via ,(s-prefix "org:organization")
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

(define-resource central-worship-service ()
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
