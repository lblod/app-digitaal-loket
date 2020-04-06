(define-resource automatic-submission-task ()
  :class (s-prefix "melding:AutomaticSubmissionTask")
  :properties `((:created :datetime ,(s-prefix "dct:created")))
  :has-one `((submission :via ,(s-prefix "prov:generated")
                         :as "submission"))
  :resource-base (s-url "http://data.lblod.info/automatic-submission-tasks/")
  :on-path "automatic-submission-tasks")

(define-resource submission ()
  :class (s-prefix "meb:Submission")
  :properties `((:href :url ,(s-prefix "prov:atLocation"))
                (:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:sent-date :datetime ,(s-prefix "nmo:sentDate"))
                (:source :url ,(s-prefix "dct:source"))
                (:received-date :datetime ,(s-prefix "nmo:receivedDate")))
  :has-one `((bestuurseenheid :via ,(s-prefix "pav:createdBy")
                              :as "organization")
             (vendor :via ,(s-prefix "pav:providedBy")
                     :as "publisher")
             (submission-document :via ,(s-prefix "dct:subject")
                                  :as "submission-document")
             (submission-document-status :via ,(s-prefix "adms:status")
                                         :as "status")
             (automatic-submission-task :via ,(s-prefix "prov:generated")
                                        :inverse t
                                        :as "task")
             (gebruiker :via ,(s-prefix "ext:lastModifiedBy")
                        :as "last-modifier")
             (form-data :via ,(s-prefix "prov:generated")
                        :as "form-data"))
  :has-many `((file :via ,(s-prefix "dct:hasPart")
                    :as "file"))
  :resource-base (s-url "http://data.lblod.info/submissions/")
  :features `(include-uri)
  :on-path "submissions")

(define-resource submission-document ()
  :class (s-prefix "ext:SubmissionDocument")
  :has-one `((submission :via ,(s-prefix "dct:subject")
                                  :inverse t
                                  :as "submission"))
  :resource-base (s-url "http://data.lblod.info/submission-documents/")
  :features `(include-uri)
  :on-path "submission-documents")

(define-resource vendor ()
  :class (s-prefix "ext:Vendor") ; Subclass of foaf:Agent
  :properties `((:name :string ,(s-prefix "foaf:name"))
                (:key :string ,(s-prefix "muAccount:key")))
  :resource-base (s-url "http://data.lblod.info/vendors/")
  :features `(include-uri)
  :on-path "vendors")

(define-resource tax-rate ()
  :class (s-prefix "lblodBesluit:TaxRate") ; Subclass of schema:UnitPriceSpecification
  :properties `((:amount :number ,(s-prefix "schema:price")))
  :resource-base (s-url "http://data.lblod.info/tax-rates/")
  :features `(include-uri)
  :on-path "tax-rates")

(define-resource form-data ()
  :class (s-prefix "melding:FormData")
  :properties `((:date-publication :datetime ,(s-prefix "eli:date_publication"))
                (:financial-year :string ,(s-prefix "elod:financialYear"))
                (:description :string ,(s-prefix "dct:description"))
                (:comment :string ,(s-prefix "rdfs:comment"))

                (:first-date-in-force :date ,(s-prefix "eli:first_date_entry_in_force"))
                (:date-no-longer-in-force :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:authenticity-type :url ,(s-prefix "lblodBesluit:authenticityType"))
                (:chart-of-account :url ,(s-prefix "lblodBesluit:chartOfAccount"))
                (:tax-type :url ,(s-prefix "lblodBesluit:taxType"))
                (:tax-rate :url ,(s-prefix "lblodBesluit:taxRate"))
                (:has-additional-tax-rate :boolean ,(s-prefix "lblodBesluit:hasAdditionalTaxRate"))
                (:link :url ,(s-prefix "dct:hasPart"))

                (:tax-rate-amount :number ,(s-prefix "ext:taxRateAmount"))
                (:session-started-at-time :datetime ,(s-prefix "ext:sessionStartedAtTime"))
                )
  :has-one `((submission :via ,(s-prefix "prov:generated")
                         :inverse t
                         :as "submission")
             (bestuurseenheid :via ,(s-prefix "eli:is_about")
                              :as "is-about")
             (bestuursorgaan :via ,(s-prefix "eli:passed_by")
                             :as "passed-by"))
  :has-many `((concept :via ,(s-prefix "dct:type")
                       :as "types"))
  :resource-base (s-url "http://data.lblod.info/form-data/")
  :features `(include-uri)
  :on-path "form-data")

;; CODELISTS

(define-resource submission-document-status () ;; subclass of skos:Concept
  :class (s-prefix "ext:SubmissionDocumentStatus")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "submission-document-statuses")

(define-resource authenticity-type ()
  :class (s-prefix "ext:AuthenticityType") ;; subclass of skos:Concept
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "authenticity-types")

(define-resource tax-type () ;; subclass of skos:Concept
  :class (s-prefix "ext:TaxType")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "tax-types")

(define-resource chart-of-account () ;; subclass of skos:Concept
  :class (s-prefix "ext:ChartOfAccount")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "chart-of-accounts")

(define-resource concept-scheme ()
  :class (s-prefix "skos:ConceptScheme")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((concept :via ,(s-prefix "skos:inScheme")
                       :inverse t
                       :as "concepts")
              (concept :via ,(s-prefix "skos:topConceptOf")
                       :inverse t
                       :as "top-concepts"))
  :resource-base (s-url "http://lblod.data.gift/concept-schemes/")
  :features `(include-uri)
  :on-path "concept-schemes"
)

(define-resource concept ()
  :class (s-prefix "skos:Concept")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((concept-scheme :via ,(s-prefix "skos:inScheme")
                              :as "concept-schemes")
              (concept-scheme :via ,(s-prefix "skos:topConceptOf")
                              :as "top-concept-schemes"))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "concepts"
)
;;END CODELISTS
