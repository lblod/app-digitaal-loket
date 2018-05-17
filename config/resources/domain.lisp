(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* nil
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")

(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-organisatie-domain.lisp")
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "master-users-domain.lisp")
(read-domain-file "master-validations-domain.lisp")
(read-domain-file "master-files-domain.lisp")
(read-domain-file "dynamic-forms-domain.lisp")
(read-domain-file "toezicht-domain.lisp")
