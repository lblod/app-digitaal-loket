(define-resource document-status ()
  :class (s-prefix "ext:DocumentStatus")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/document-statuses/")
  :features `(no-pagination-defaults include-uri)
  :on-path "document-statuses")

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:filename :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:download-url :url ,(s-prefix "nie:url"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download")
              (file-address :via ,(s-prefix "nie:dataSource")
                            :as "data-source")
              (data-container :via ,(s-prefix "task:hasFile")
                :inverse t
                :as "data-container")
              (email :via ,(s-prefix "email:hasEmail")
                  :as "email"))
  :resource-base (s-url "http://data.lblod.info/files/")
  :features `(no-pagination-defaults include-uri)
  :on-path "files")

(define-resource file-address ()
  :class (s-prefix "ext:FileAddress")
  :properties `((:address :url ,(s-prefix "ext:fileAddress")))
  :has-one `(
              (file :via ,(s-prefix "nie:dataSource")
                    :inverse t
                    :as "replicated-file"))
  :resource-base (s-url "http://data.lblod.info/file-addresses/")
  :features `(no-pagination-defaults include-uri)
  :on-path "file-addresses")

(define-resource remote-url ()
 :class (s-prefix "nfo:RemoteDataObject")
 :properties `((:address :url ,(s-prefix "nie:url"))
               (:created :datetime ,(s-prefix "dct:created"))
               (:modified :datetime ,(s-prefix "dct:modified"))
               (:download-status :url ,(s-prefix "adms:status"))
               (:creator :url ,(s-prefix "dct:creator"))
               )
 :has-one `(
   (file :via ,(s-prefix "nie:dataSource")
                  :inverse t
                  :as "download")
                  )
 :resource-base (s-url "http://lblod.data.gift/id/remote-urls/")
 :features `(include-uri)
 :on-path "remote-urls")

(define-resource remote-data-object ()
  :class (s-prefix "nfo:RemoteDataObject")
  :properties `((:source :url ,(s-prefix "nie:url"))
                (:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:request-header :url ,(s-prefix "rpioHttp:requestHeader"))
                (:status :url ,(s-prefix "adms:status"))
                (:comment :string ,(s-prefix "skos:comment"))
                (:creator :url ,(s-prefix "dct:creator")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
              :inverse t
              :as "file")
            (harvesting-collection :via ,(s-prefix "dct:hasPart")
              :inverse t
              :as "harvesting-collection")
            (authentication-configuration :via ,(s-prefix "dgftSec:targetAuthenticationConfiguration")
              :as "authentication-configuration"))
  :resource-base (s-url "http://data.lblod.info/id/remote-data-objects/")
  :features `(include-uri)
  :on-path "remote-data-objects")

;; 
;; Authentication Configuration
;; 

(define-resource authentication-configuration ()
  :class (s-prefix "dgftSec:AuthenticationConfiguration")
  :has-one `((credential :via ,(s-prefix "dgftSec:secrets")
              :as "credential")
            (security-scheme :via ,(s-prefix "dgftSec:securityConfiguration")
              :as "security-scheme"))
  :resource-base (s-url "http://data.lblod.info/id/authentication-configurations/")
  :features `(include-uri)
  :on-path "authentication-configurations")

;; 
;; Secrets (Security Credentials)
;; 

(define-resource credential ()
  ;; Abstract superclass, so that it can be used in the frontend
  :class (s-prefix "dgftSec:Credentials")
  :resource-base (s-url "http://data.lblod.info/id/credentials/")
  :features '(include-uri)
  :on-path "credentials")

(define-resource basic-authentication-credential (credential)
  :class (s-prefix "dgftSec:BasicAuthenticationCredentials")
  :properties `((:username :string ,(s-prefix "meb:username"))
                (:password :string ,(s-prefix "muAccount:password")))
  :resource-base (s-url "http://data.lblod.info/id/basic-authentication-credentials/")
  :features '(include-uri)
  :on-path "basic-authentication-credentials")

(define-resource oauth2-credential (credential)
  :class (s-prefix "dgftSec:OAuth2Credentials")
  :properties `((:client-id      :string ,(s-prefix "dgftOauth:clientId"))
                (:client-secret  :string ,(s-prefix "dgftOauth:clientSecret")))
  :resource-base (s-url "http://data.lblod.info/id/oauth-2-credentials/")
  :features '(include-uri)
  :on-path "oauth2-credentials")

;;
;; Security Schemes
;;

(define-resource security-scheme ()
  ;; Abstract superclass
  :class (s-prefix "wotSec:SecurityScheme")
  :resource-base (s-url "http://data.lblod.info/id/security-schemes/")
  :features '(include-uri)
  :on-path "security-schemes")

(define-resource basic-security-scheme (security-scheme)
  :class (s-prefix "wotSec:BasicSecurityScheme")
  :resource-base (s-url "http://data.lblod.info/id/basic-security-schemes/")
  :features '(include-uri)
  :on-path "basic-security-schemes")

(define-resource oauth2-security-scheme (security-scheme)
  :class (s-prefix "wotSec:OAuth2SecurityScheme")
  :properties `((:token :string ,(s-prefix "wotSec:token"))
                (:flow  :string ,(s-prefix "wotSec:flow")))
  :resource-base (s-url "http://data.lblod.info/id/oauth2-security-schemes/")
  :features '(include-uri)
  :on-path "oauth2-security-schemes")