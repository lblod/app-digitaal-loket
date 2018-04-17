(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")

(read-domain-file "master-mandaat-domain.lisp")
(read-domain-file "master-organisatie-domain.lisp")
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "master-users-domain.lisp")

