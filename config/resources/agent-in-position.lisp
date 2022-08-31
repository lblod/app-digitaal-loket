;; re-shuffle declaration of files, because
;; mu-resource 1.21.0 is sensible when files
;; are declared vs loaded the class hierarchy
;; hence this is a temporary workaround
;; ORDER REALLY MATTERS FOR NOW!

;;"RESHUFFLED" from worship-units.lisp
(define-resource agent-in-position ()
  :class (s-prefix "ch:AgentInPositie")
  :properties `((:agent-start-date :date ,(s-prefix "ch:startdatum"))
                (:agent-end-date :date ,(s-prefix "ch:eindedatum")))
  :has-one `((post :via ,(s-prefix "org:holds")
                   :as "post")
             (persoon :via ,(s-prefix "org:heldBy")
                      :as "person")
             ;;NOTE: this relation belongs to a child class
             ;; This is a workaround to allow filtering on properties of child-class, which come through the parent-class.
             (persoon :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                      :as "is-bestuurlijke-alias-van")
             )
  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contacts"))
  :resource-base (s-url "http://data.lblod.info/id/agentenInPositie/")
  :features '(include-uri)
  :on-path "agents-in-position"
)

;;"RESHUFFLED" from slave-mandaat-domain.lisp
(define-resource mandataris (agent-in-position)
  :class (s-prefix "mandaat:Mandataris")
  :properties `((:rangorde :string ,(s-prefix "mandaat:rangorde"))
                (:start :datetime ,(s-prefix "mandaat:start"))
                (:einde :datetime ,(s-prefix "mandaat:einde"))
                (:datum-eedaflegging :datetime ,(s-prefix "ext:datumEedaflegging"))
                (:datum-ministrieel-besluit :datetime ,(s-prefix "ext:datumMinistrieelBesluit"))
                (:generated-from :uri-set ,(s-prefix "ext:generatedFrom")) ;;if it e.g. comes from gelinkt-notuleren
                (:duplication-reason :string ,(s-prefix "skos:changeNote")))
  :has-many `((rechtsgrond-aanstelling :via ,(s-prefix "mandaat:isAangesteldDoor")
                                       :as "rechtsgronden-aanstelling")
              (rechtsgrond-beeindiging :via ,(s-prefix "mandaat:isOntslagenDoor")
                                       :as "rechtsgronden-beeindiging")
              (mandataris :via ,(s-prefix "mandaat:isTijdelijkVervangenDoor")
                          :as "tijdelijke-vervangingen")
              (contact-point :via ,(s-prefix "schema:contactPoint")
                          :as "contact-points")
              (beleidsdomein-code :via ,(s-prefix "mandaat:beleidsdomein")
                                  :as "beleidsdomein"))
  :has-one `((mandaat :via ,(s-prefix "org:holds")
                      :as "bekleedt")
             (lidmaatschap :via ,(s-prefix "org:hasMembership")
                           :as "heeft-lidmaatschap")
             (persoon :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                      :as "is-bestuurlijke-alias-van")
             (mandataris-status-code :via ,(s-prefix "mandaat:status")
                                     :as "status")
             (mandataris :via ,(s-prefix "owl:sameAs")
                         :as "duplicate-of"))
  :resource-base (s-url "http://data.lblod.info/id/mandatarissen/")
  :features '(include-uri)
  :on-path "mandatarissen")

;;"RESHUFFLED" from slave-mandaat-domain.lisp
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

;;"RESHUFFLED" from whorship-units.lisp
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
