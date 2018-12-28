(in-package :mu-cl-resources)

(define-resource form-node ()
  :class (s-prefix "ext:FormNode")
  :properties `((:input-type-map :string ,(s-prefix "ext:typeMap")))
  :has-many `((form-input :via ,(s-prefix "ext:formInput")
                          :as "children")
              (dynamic-subform :via ,(s-prefix "ext:hasFormNode")
                               :inverse t
                               :as "parents"))
  :resource-base (s-url "http://data.lblod.info/form-nodes/")
  :on-path "form-nodes")

(define-resource form-input ()
  :class (s-prefix "ext:FormInput")
  :properties `((:index :number ,(s-prefix "ext:index"))
                (:display-type :string ,(s-prefix "ext:displayType"))
                (:label :string ,(s-prefix "dct:title"))
                (:options :string ,(s-prefix "ext:options"))
                (:identifier :string ,(s-prefix "adms:identifier")))
  :has-many `((dynamic-subform :via ,(s-prefix "ext:dynamicSubforms")
                               :as "dynamic-subforms")
              (input-state :via ,(s-prefix "ext:inputStates")
                           :as "input-states"))
  :resource-base (s-url "http://data.lblod.info/form-inputs/")
  :on-path "form-inputs")

(define-resource input-state ()
  :class (s-prefix "ext:InputState")
  :properties `((:validation-name :string ,(s-prefix "ext:validationName"))
                (:state-name :string ,(s-prefix "ext:stateName")))
  :resource-base (s-url "http://data.lblod.info/input-states/")
  :on-path "input-states")

(define-resource dynamic-subform ()
  :class (s-prefix "ext:DynamicSubform")
  :properties `((:key :string ,(s-prefix "ext:key"))
                ;; match-kind is defined for resource properties.
                ;; Should be "uri" to match on object's uri property,
                ;; or "uuid" to match on its identifier.
                (:match-kind :string ,(s-prefix "ext:matchKind"))
                (:value :string ,(s-prefix "ext:value")))
  :has-one `((form-node :via ,(s-prefix "ext:hasFormNode")
                        :as "form-node"))
  :resource-base (s-url "http://data.lblod.info/dynamic-subforms/")
  :on-path "dynamic-subforms")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic form needs a form-solution which contains a relation specific to your domain
;;(define-resource form-solution ()
;;  :class (s-prefix "ext:FormSolution")
;;  :properties `((:has-owner :string ,(s-prefix "ext:hasOwnerAsString")))
;;  :has-one `((form-node :via ,(s-prefix "ext:hasForm")
;;                        :as "form-node")
;;
;; This following relation needs to be customized for your domain.
;;             (company :via ,(s-prefix "ext:hasCompany")
;;                      :as "company"))
;;
;;  :resource-base (s-url "http://data.lblod.info/form-solutions/")
;;  :on-path "form-solutions")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
