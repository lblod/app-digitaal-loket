;; re-shuffle declaration of files, because
;; mu-resource 1.21.0 is sensible when files
;; are declared vs loaded the class hierarchy
;; hence this is a temporary workaround
;; ORDER REALLY MATTERS FOR NOW!

;;"RESHUFFLED" from slave-besluit.lisp
(define-resource bestuurseenheid () ;; Subclass of m8g:PublicOrganisation, which is a subclass of dct:Agent
  :class (s-prefix "besluit:Bestuurseenheid")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel"))
                (:alternatieve-naam :string-set ,(s-prefix "skos:altLabel"))
                (:wil-mail-ontvangen :boolean ,(s-prefix "ext:wilMailOntvangen")) ;;Voorkeur in berichtencentrum
                (:mail-adres :string ,(s-prefix "ext:mailAdresVoorNotificaties"))
                (:is-trial-user :boolean ,(s-prefix "ext:isTrailUser"))
                (:uses-new-loket :boolean ,(s-prefix "ext:usesNewLoket"))
                (:view-only-modules :string-set ,(s-prefix "ext:viewOnlyModules")))

  :has-one `((werkingsgebied :via ,(s-prefix "besluit:werkingsgebied")
                             :as "werkingsgebied")
             (werkingsgebied :via ,(s-prefix "ext:inProvincie")
                             :as "provincie")
             (bestuurseenheid-classificatie-code :via ,(s-prefix "besluit:classificatie")
                                                 :as "classificatie"))

  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contactinfo")
              (bestuursorgaan :via ,(s-prefix "besluit:bestuurt")
                              :inverse t
                              :as "bestuursorganen")
              (vendor :via ,(s-prefix "muAccount:canActOnBehalfOf")
                              :inverse t
                              :as "vendors")
              (participation :via ,(s-prefix "m8g:playsRole")
                            :as "participations"))

  :resource-base (s-url "http://data.lblod.info/id/bestuurseenheden/")
  :features '(include-uri)
  :on-path "bestuurseenheden"
)

;;"RESHUFFLED" from worship-units.lisp
(define-resource worship-administrative-unit (bestuurseenheid)
  :class (s-prefix "ere:EredienstBestuurseenheid")
  :has-one `((recognized-worship-type :via ,(s-prefix "ere:typeEredienst")
                                      :as "recognized-worship-type"))
  :has-many `((minister-position :via ,(s-prefix "ere:wordtBediendDoor")
                                 :as "minister-positions"))
  :resource-base (s-url "http://data.lblod.info/id/eredienstBestuurseenheden/")
  :features '(include-uri)
  :on-path "worship-administrative-units"
)

;;"RESHUFFLED" from worship-units.lisp
(define-resource worship-service (worship-administrative-unit)
  :class (s-prefix "ere:BestuurVanDeEredienst")
  :properties `((:denomination :string ,(s-prefix "ere:denominatie"))
                (:cross-border :boolean ,(s-prefix "ere:grensoverschrijdend")))
  :has-one `((local-involvement :via ,(s-prefix "org:organization")
                          :as "local-involvement"))
  :resource-base (s-url "http://data.lblod.info/id/besturenVanDeEredienst/")
  :features '(include-uri)
  :on-path "worship-services"
)
