(define-resource validation-execution ()
  :class (s-prefix "validation:Execution")
  :properties `((:status :string ,(s-prefix "validation:status"))
                (:validation-set :uri-set ,(s-prefix "validation:validationSet"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-many `((validation-error :via ,(s-prefix "validation:producedBy")
                                :inverse t
                                :as "errors")
              (validation :via ,(s-prefix "validation:performsValidation")
                       :as "validations"))
  :resource-base (s-url "http://mu.semte.ch/services/validation-service/executions/")
  :features '(include-uri)
  :on-path "validation-executions"
)

(define-resource validation ()
  :class (s-prefix "validation:Validation")
  :properties `((:name :string ,(s-prefix "validation:name"))
                (:description :string ,(s-prefix "validation:description"))                
                (:status :string ,(s-prefix "validation:status")))
  :has-one `((validation-execution :via ,(s-prefix "validation:performsValidation")
                                   :inverse t
                                   :as "execution"))
  :has-many `((validation-error :via ,(s-prefix "validation:validation")
                                   :inverse t
                                   :as "errors"))
  :resource-base (s-url "http://mu.semte.ch/services/validation-service/validations/")
  :features '(include-uri)
  :on-path "validations"
)


(define-resource validation-error ()
  :class (s-prefix "validation:Error")
  :properties `((:message :string ,(s-prefix "validation:message")))
  :has-one `((validation-execution :via ,(s-prefix "validation:producedBy")
                                   :as "execution")
             (validation :via ,(s-prefix "validation:validation")
                                   :as "validation"))             
  :resource-base (s-url "http://mu.semte.ch/services/validation-service/validation-errors/")
  :features '(include-uri)
  :on-path "validation-errors"
)
