(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)

(read-domain-file "master-users-domain.lisp")
(read-domain-file "master-validations-domain.lisp")
(read-domain-file "master-files-domain.lisp")
(read-domain-file "master-dynamic-forms-domain.lisp")
(read-domain-file "master-messages-domain.lisp")
(read-domain-file "master-email-domain.lisp")
(read-domain-file "master-bbcdr-domain.lisp")
(read-domain-file "master-log-domain.lisp")
(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-leidinggevenden-domain.lisp")
(read-domain-file "slave-toezicht-domain.lisp")
(read-domain-file "slave-publicatie-gn-domain.lisp")
