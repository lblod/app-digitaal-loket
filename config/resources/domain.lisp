(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* t)
(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* t)

(read-domain-file "master-users-domain.lisp")
(read-domain-file "master-validations-domain.lisp")
(read-domain-file "master-files-domain.lisp")
(read-domain-file "master-dynamic-forms-domain.lisp")
(read-domain-file "master-messages-domain.lisp")
(read-domain-file "master-email-domain.lisp")
(read-domain-file "master-bbcdr-domain.lisp")
(read-domain-file "master-personeelsdatabank-domain.lisp")
(read-domain-file "master-log-domain.lisp")
(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-leidinggevenden-domain.lisp")
(read-domain-file "slave-contact-domain.lisp")
(read-domain-file "slave-publicatie-gn-domain.lisp")
(read-domain-file "master-reports-domain.lisp")
(read-domain-file "master-submissions-domain.lisp")
(read-domain-file "master-subsidies-domain.lisp")
(read-domain-file "master-job-domain.lisp")
(read-domain-file "dcat.json")
(read-domain-file "concept-scheme.json")
(read-domain-file "lpdc.json")

(before (:list file) (resource)
  (let ((request-filters-on-uri
          (some (lambda (args)
                  (let ((components (getf args :components)))
                    (and (= 1 (length components))
                         (string= (elt components 0)
                                  ":uri:"))))
                (extract-filters-from-request))))
    (if request-filters-on-uri
        resource
        (error 'access-denied :operation :list :resource resource :id :none))))
