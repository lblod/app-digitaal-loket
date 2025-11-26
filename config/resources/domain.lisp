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
(setf *fetch-all-types-in-construct-queries* t)

(setf sparql:*query-log-types* nil) ;; added by madnificent on QA for test


;;;; START OF SECTION ADDED BY MADNIFICENT ON QA FOR TEST
(in-package :mu-support)

;; (defmethod fuseki::query-raw ((repos mu-semtech-repository) (query string) &rest options &key &allow-other-keys)
;;   (flet ((execute ()
;;            (cl-fuseki::flush-updates repos)
;;            (let ((full-query (apply #'cl-fuseki::query-update-prefixes query options)))
;;              (fuseki::maybe-log-query full-query)
;;              (send-dex-request (cl-fuseki::query-endpoint repos)
;;                                :query full-query))))
;;     (let ((retries 5)
;;           (timeout 1)
;;           (timeout-increase 5))
;;       (loop for retry from 0
;;             do
;;                (if (= retries 0)
;;                    (return (execute))
;;                    (handler-case
;;                        (if (= retry 0)
;;                            ;; aggressive first try
;;                            ;; TODO: inform mu-auth about this so it can optimize
;;                            (let ((dexador.util:*default-connect-timeout* 5)
;;                                  (dexador.util:*default-read-timeout* 10))
;;                              (return (execute)))
;;                            ;; lenient afterwards
;;                            (let ((dexador.util:*default-connect-timeout* 55)
;;                                  (dexador.util:*default-read-timeout* 120))
;;                              (return (execute))))
;;                      (usocket:socket-error (e)
;;                        (format t "~&Socket error (try ~A), retrying~%~A~%" (1+ retry) e)
;;                        (sleep timeout)
;;                        (setf timeout (+ timeout timeout-increase))
;;                        (decf retries))
;;                      (sb-sys:io-timeout (e)
;;                        (format t "~&IO timeout error (try ~A), retrying~%~A~%" (1+ retry) e)
;;                        (sleep timeout)
;;                        (setf timeout (+ timeout timeout-increase))
;;                        (decf retries))))))))

(in-package :mu-cl-resources)

;; (defun send-dex-request (url &key (wanted-status-codes '(200)) (query "") &allow-other-keys)
;;   ;; (cl-fuseki::remove-key html-args :wanted-status-codes)
;;   (multiple-value-bind (response status-code response-headers)
;;       (let ((url (quri:uri url))
;;             (headers `(("accept" . "application/sparql-results+json")
;;                        ,@(alexandria:when-let ((header (hunchentoot:header-in* :mu-session-id)))
;;                            `(("mu-session-id" . ,header)))
;;                        ,@(alexandria:when-let ((header (hunchentoot:header-in* :mu-call-id)))
;;                            `(("mu-call-id" . ,header)))
;;                        ,@(alexandria:when-let
;;                              ((allowed-groups (or (hunchentoot:header-out :mu-auth-allowed-groups)
;;                                                   (hunchentoot:header-in* :mu-auth-allowed-groups))))
;;                            (list (cons "mu-auth-allowed-groups" allowed-groups))))))
;;         (if (< (length query) 5000) ;; guessing on 5k here
;;             (progn
;;               (setf (quri:uri-query-params url)
;;                     `(("query" . ,query)))
;;               (dex:request url
;;                            :method :get
;;                            :use-connection-pool t
;;                            :keep-alive t
;;                            :force-string t
;;                            ;; :verbose t
;;                            :headers headers))
;;             (dex:request url
;;                          :method :post
;;                          :use-connection-pool t
;;                          :keep-alive nil
;;                          :force-string t
;;                          :headers headers
;;                          :content `(("query" . ,query)))))
;;     (unless (and wanted-status-codes
;;                  (find status-code wanted-status-codes))
;;       (error 'cl-fuseki:sesame-exception
;;              :status-code status-code
;;              :response response))

;;     ;; make sure the authorization keys are set on the response
;;     (alexandria:when-let ((received-auth-allowed-groups
;;                            (gethash "mu-auth-allowed-groups" response-headers)))
;;       (setf (hunchentoot:header-out :mu-auth-allowed-groups)
;;             received-auth-allowed-groups))
;;     (alexandria:when-let ((received-auth-used-groups
;;                            (gethash "mu-auth-used-groups" response-headers)))
;;       ;; TODO: this second one is incorrect, we should actually join
;;       ;; the used groups.  this will take a bit more time and the
;;       ;; joining is not necessary yet.
;;       (setf (hunchentoot:header-out :mu-auth-used-groups)
;;             received-auth-used-groups))
;;     response))

(in-package :mu-cl-resources)

(setf *warn-on-single-valued-slot-with-multiple-values* nil)

;;;; END OF SECTION ADDED BY MADNIFICENT ON QA FOR TEST


;; Note: the organisation of the files is currently a bit of a mess.
;; Previously we ordered files by Application Profile/Model.
;; However since the introduction of inheritance of mu-resource
;; we had to shuffle the declaration of resources around
;; since there is a bug in mu-resource 1.21.0 that forces us
;; to declare the inheritance tree in one file.
;; This means that we had to break the content of some files
;; and move these to other files.
;; To track these resource, look for the files with comment "RESHUFFLED".

(read-domain-file "agent-in-position.lisp")
(read-domain-file "organisation.lisp")
(read-domain-file "concept-scheme.lisp")
(read-domain-file "post.lisp")
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
(read-domain-file "worship-units.lisp")
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-leidinggevenden-domain.lisp")
(read-domain-file "slave-contact-domain.lisp")
(read-domain-file "slave-publicatie-gn-domain.lisp")
(read-domain-file "master-reports-domain.lisp")
(read-domain-file "master-submissions-domain.lisp")
(read-domain-file "master-job-domain.lisp")
(read-domain-file "master-harvest-domain.lisp")
(read-domain-file "dcat.json")

;; Extra security layer to return 403 on GET /files
;; It should be ok for mu-auth; but devs can make bugs and add files to the wrong graph (i.e. public)
(before (:list file) (resource)
  (let ((request-filters-on-uri
          (some (lambda (args)
                  (let ((components (getf args :components)))

                    ;;matches /files?filter[data-container][input-from-tasks][:id:]=''
                    (or
                      (and (= 3 (length components))
                           (string= (elt components 2)
                                    ":id:"))

                     ;;matches /files?filter[:uri:]=''
                       (and (= 1 (length components))
                           (string= (elt components 0)
                                    ":uri:")))
                    ))
                (extract-filters-from-request))))
    (if request-filters-on-uri
        resource
        (error 'access-denied :operation :list :resource resource :id :none))))
