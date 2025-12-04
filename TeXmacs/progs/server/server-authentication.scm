
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-authentication.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-authentication)
  (:use (database db-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (slog lvl channel msg)
  (if (headless?)
    (server-log-write-int lvl msg)
    (debug-message channel msg)))

(tm-define (server-log-write level s)
  (with msg (if (string-ends? s "\n") s (string-append s "\n"))
    (cond
      ((== level `emergency) (slog 0 "server-error" msg))
      ((== level `alert) (slog 1 "server-error" msg))
      ((== level `critical) (slog 2 "server-error" msg))
      ((== level `error) (slog 3 "server-error" msg))
      ((== level `warning) (slog 4 "server-warning" msg))
      ((== level `notice) (slog 5 "server-debug" msg))
      ((== level `info) (slog 6 "server-debug" msg))
      (else (slog 7 "server-debug" msg)))))

(tm-define (server-log-format level . args)
  (server-log-write level (apply format (cons #f args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-server-port-preferences var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Server port is set to " val))))

(define (notify-server-service-public-preferences var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing public preferences turned " val))))

(define (notify-server-service-admin-preferences var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing admin preferences turned " val))))

(define (notify-server-service-set-preferences var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing setting preferences turned " val))))

(define (notify-server-service-new-account var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing remote account creation turned " val))))

(define (notify-server-service-login var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing remote login via password " val))))

(define (notify-tls-server var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Turning TLS " val))))

(define (notify-tls-server-authentication-anonymous var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Turning anonymous authentication via TLS " val))))

(define (notify-server-contact-timeout var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Server contact timeout is set to " val))))

(define (notify-server-connection-timeout var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Server connection timeout is set to " val))))

(define (notify-server-failed-login-limit var val)
;; 0 means no limit
  (when (server-started?)
    (server-log-write `info
      (string-append "Maximal number of failed login set to " val))))

(define (notify-server-failed-login-delay var val)
  ;; in seconds, -1 means infinity
  (when (server-started?)
    (server-log-write `info
      (string-append "Login delay for failure limit is set to " val))))

(define (notify-server-require-strong-passwords var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Turning strong password requirement " val))))

(define (notify-server-account-confirmation-delay var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Account confirmation delay is set to " val))))

(define (notify-server-service-reset-credentials var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Allowing to reset credentials turned " val))))

(define (notify-server-reset-credentials-delay var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Credentials resetting delay set to " val))))

(define (notify-server-mail-command var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Mail command set to " val))))

(define-preferences
  ("server port" "6561"
   notify-server-port-preferences)
  ("server service public preferences" "on"
   notify-server-service-public-preferences)
  ("server service admin preferences" "on"
   notify-server-service-admin-preferences)
  ("server service set preferences" "on"
   notify-server-service-set-preferences)
  ("server service new-account" "off"
   notify-server-service-new-account)
  ("server service login" "on"
   notify-server-service-login)
  ("tls-server" "on"
   notify-tls-server)
  ("tls-server authentication anonymous" "on"
   notify-tls-server-authentication-anonymous)
  ("server contact timeout" "10000" ;; 10s
   notify-server-contact-timeout)
  ("server connection timeout" "120" ;; 120s
   notify-server-connection-timeout)
  ("server failed login limit" "3"
   notify-server-failed-login-limit)
  ("server failed login delay" "3600" ;; 1h
   notify-server-failed-login-delay)
  ("server require strong passwords" "on"
   notify-server-require-strong-passwords)
  ("server account confirmation delay" "-1" ;; no confirmation
   notify-server-account-confirmation-delay)
  ("server service reset-credentials" "off"
   notify-server-service-reset-credentials)
  ("server reset-credentials delay" "3600" ;; 1h
   notify-server-reset-credentials-delay)
  ("server mail command" ""
   notify-server-mail-command))

(tm-define (server-get-port)
  (:synopsis "Port to run the server")
  (get-preference "server port"))

(tm-define (server-set-port port)
  (:synopsis "Set port to run the server")
  (:argument port "Port number")
  (set-preference "server port" port))

(tm-define (server-get-contact-timeout)
  (string->number (get-preference "server contact timeout")))

(tm-define (server-set-contact-timeout val)
  (:interactive)
  (:argument val "Contact timeout in ms")
  (when (or (number? val) (string-number? val))
    (set-preference "server contact timeout" val)))

(tm-define (server-get-connection-timeout)
  (string->number (get-preference "server connection timeout")))

(tm-define (server-set-connection-timeout val)
  (:interactive)
  (:argument val "Connection timeout in seconds")
  (when (or (number? val) (string-number? val))
    (set-preference "server connection timeout" val)))

(tm-define (server-get-failed-login-limit)
  (string->number (get-preference "server failed login limit")))

(tm-define (server-set-failed-login-limit val)
  (:interactive)
  (:argument val "Failed login limit")
  (when (or (number? val) (string-number? val))
    (set-preference "server failed login limit" val)))

(tm-define (server-get-failed-login-delay)
  (string->number (get-preference "server failed login delay")))

(tm-define (server-set-failed-login-delay val)
  (:interactive)
  (:argument val "Failed login delay in seconds")
  (when (or (number? val) (string-number? val))
    (set-preference "server failed login delay" val)))

(tm-define (server-get-account-confirmation-delay)
  (string->number (get-preference "server account confirmation delay")))

(tm-define (server-set-account-confirmation-delay val)
  (:interactive)
  (:argument val "Account confirmation delay in seconds")
  (when (or (number? val) (string-number? val))
    (set-preference "server account confirmation delay" val)))

(tm-define (server-get-reset-credentials-delay)
  (string->number (get-preference "server reset-credentials delay")))

(tm-define (server-set-reset-credentials-delay val)
  (:interactive)
  (:argument val "Credentials resetting delay in seconds")
  (when (or (number? val) (string-number? val))
    (set-preference "server reset-credentials delay" val)))

(tm-define (server-get-mail-command) (get-preference "server mail command"))

(tm-define (server-admin-preferences)
  (with server-pref?
    (lambda (pref value prior)
      ; (display* pref value prior "\n")
      (if (and (or (string-prefix? "server" pref)
                   (string-prefix? "tls-server" pref))
               (not (string-prefix? "server public:" pref)))
        (acons pref (get-preference pref) prior)
        prior))
    (ahash-fold server-pref? '() preferences-default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public server preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All preferences
(define server-all-preferences
  (list "server port"
	;"server service new-account"
	"server service login"
	"tls-server"
	"tls-server authentication anonymous"))
	;"server contact timeout"
	;"server connection timeout"
	;"server password encoding"
	;"server password update"))

;; Preferences are all publicly available by default
(for (x server-all-preferences)
  (define-preferences
    ((string-append "server public: " x) "on" noop)))

(tm-define (server-public-preferences)
  (with ret `()
    (for (x server-all-preferences)
      (when (== (get-preference (string-append "server public: " x)) "on")
	(set! ret (cons (list x (get-preference x)) ret))))
    (and-with doc (server-get-license)
      (set! ret (cons (list "server license" doc) ret)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strong passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define server-strong-password-instructions
  (string-append "passwords must contain at least 10 characters, "
		 "one upper case, one lower case, one digit, and one symbol"))

(define char-set:password-symbol
  (string->char-set "!\"$%^&*()_-+={}[]:;@`~#|<,>.?/\\"))

(tm-define (server-strong-password? p)
  (with c (string->char-set p)
  (and (>= (string-length p) 10)
       (>= (char-set-size (char-set-intersection char-set:lower-case c))
	   1)
       (>= (char-set-size (char-set-intersection char-set:upper-case c))
	   1)
       (>= (char-set-size (char-set-intersection char-set:digit c))
	   1)
       (>= (char-set-size (char-set-intersection char-set:password-symbol c))
	   1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (password-parse-crypt-style hash)
  (let* ((purified (car (string-decompose hash "\n")))
         (fields (cdr (string-decompose purified "$")))
         (algo (car fields)))
    ;(display* "parsed crypt style: " fields " from " purified "\n")
    (cond ((== algo "5") `(password "sha256" . ,(cdr fields)))
          ((== algo "6") `(password "sha512" . ,(cdr fields)))
          ; define 7 as pbkdf2, put the iteration (third field) at the end
          ((== algo "7")
           `(password "pbkdf2" . ,(append (cddr fields) (list (cadr fields)))))
          (else #f))))

;; Clear text
(define (password-supports-clear?)
  #t)

(define (password-encode-clear p)
  `(password "clear" ,p))

(define (password-correct-clear? p hidden)
  (== p (third hidden)))

;; SHA256 based algorithm
(define (password-encode-sha256 p)
  (with (ret out err)
      (evaluate-system (list "openssl" "passwd" "-5" "-stdin")
		       (list 0) (list p) (list 1 2))
    (if (or (!= ret "0") (!= err "")) #f (password-parse-crypt-style out))))

(define (password-correct-sha256? p hidden)
  (let* ((salt (third hidden))
         (enc (fourth hidden)))
    (with  (ret out err)
	(evaluate-system (list "openssl" "passwd" "-5" "-salt" salt "-stdin")
			 (list 0) (list p) (list 1 2))
      (if (or (!= ret "0") (!= err ""))
	  #f
          (with l (password-parse-crypt-style out)
	    (and (>= (length l) 4) (== (fourth l) enc)))))))

(define (password-supports-sha256?)
  (and (not (os-mingw?)) (password-encode-sha256 "foo")))

;; SHA512 based algorithm
(define (password-encode-sha512 p)
  (with (ret out err)
      (evaluate-system (list "openssl" "passwd" "-6" "-stdin")
		       (list 0) (list p) (list 1 2))
    (if (or (!= ret "0") (!= err "")) #f (password-parse-crypt-style out))))

(define (password-correct-sha512? p hidden)
  (display* "verifying " p ", hidden: " hidden "\n")
  (let* ((salt (third hidden))
         (enc (fourth hidden)))
    (with  (ret out err)
	(evaluate-system (list "openssl" "passwd" "-6" "-salt" salt "-stdin")
			 (list 0) (list p) (list 1 2))
      (if (or (!= ret "0") (!= err ""))
	  #f
          (with l (password-parse-crypt-style out)
                (display* l "\n")
                (and (>= (length l) 4) (== (fourth l) enc)))))))

(define (password-supports-sha512?)
  (and (not (os-mingw?)) (password-encode-sha512 "foo")))

;; PBKDF2 with HMAC 256 algorithm using GnuTLS
;; the return of our internal gnutls pbkdf2 is $7$iter$salt$pwd$
;; so we put the iter at the end of this password encoding
(define (password-encode-pbkdf2 p salt)
  (if (not (supports-gnutls?)) #f
    (with out (hash-password-pbkdf2 p salt) (password-parse-crypt-style out))))

(define (password-correct-pbkdf2? p hidden)
  (let* ((salt (third hidden))
         (enc  (fourth hidden)))
    (with l (password-encode-pbkdf2 p salt)
          ; (display* "pwd to check: " p ", hash to check: " l ", hidden: " hidden "\n")
          (and (>= (length l) 4) (== (fourth l) enc)))))

(define (password-supports-pbkdf2?)
  (and (password-encode-pbkdf2
         "foo"
         "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWE=")))

;; List all supported password encodings
(tm-define (server-supported-password-encodings)
  (with l (list "clear")
    (when (password-supports-sha256?)
      (set! l (cons "sha256" l)))
    (when (password-supports-sha512?)
      (set! l (cons "sha512" l)))
    (when (password-supports-pbkdf2?)
      (set! l (cons "pbkdf2" l)))
    l))

(tm-define (server-password-encode p salt type)
  (cond ((== type "clear") (password-encode-clear p))
	((== type "sha256") (password-encode-sha256 p))
	((== type "sha512") (password-encode-sha512 p))
	((== type "pbkdf2") (password-encode-pbkdf2 p salt))
	(else #f)))

(tm-define (server-password-correct? p hidden)
  (cond ((or (nlist? hidden) (< (length hidden) 3)
	     (!= (car hidden) `password)) #f)
	((== (second hidden) "clear") (password-correct-clear? p hidden))
	((== (second hidden) "sha256") (password-correct-sha256? p hidden))
	((== (second hidden) "sha512") (password-correct-sha512? p hidden))
	((== (second hidden) "pbkdf2") (password-correct-pbkdf2? p hidden))
	(else #f)))

;; Preferred password encoding
(define (notify-server-password-encoding var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Turning password encoding to " val))))

(define-preferences
  ("server password encoding" (car (server-supported-password-encodings))
  notify-server-password-encoding))

(tm-define (server-password-encoding)
  (get-preference "server password encoding"))

;; Update password encoding at login
(define (notify-server-password-update var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Turning password updating " val))))

(define-preferences
  ("server password update" "on" notify-server-password-update))

(tm-define (server-password-update?)
  (== (get-preference "server password update") "on"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Credential storage (hidden form)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For backward compatibility
(tm-define (server-credential-normalize credential)
  (if (string? credential) `(password "clear" ,credential) credential))

(tm-define (server-credentials-normalize credentials)
  (if (string? credentials)
      (list `(password "clear" ,credentials))
      (if (list? credentials)
	  (if (null? credentials)
	      credentials
	      (with credential (car credentials)
		(if (and (list? credential) (<= (length credential) 1))
		    (cdr credentials)
		    (cons credential
			  (server-credentials-normalize
			   (cdr credentials)))))))))

(tm-define (server-hide-credential cred)
  ;(display* "hiding credentials " cred "\n")
  (let ((type (car cred))
       (password (second cred))
       (salt (third cred)))
  (cond
    ((or (== type `password) (== type `tls-password))
     (server-password-encode password salt (server-password-encoding)))
    (else cred))))

(tm-define (server-hide-credentials credentials)
  (map server-hide-credential credentials))

(tm-define (server-add-salt credentials salt)
  (map (lambda (cred) (if (list-2? cred) (append cred `(,salt)) cred))
       credentials))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Credential edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-credential-password? cred)
  (and (list? cred) (nnull? cred) (== (car cred) `password)))

(tm-define (server-credentials-remove credentials type)
  (filter (lambda (z) (not (func? z type))) credentials))

(tm-define (server-credential-update credentials credential)
  (let* ((type (first credential))
	 (credentials2 (server-credentials-remove
			(server-credentials-normalize credentials)
			type)))
    (when (list? credential)
      (set! credentials2
	    (cons (server-hide-credential credential)
		  credentials2)))
    credentials2))

(tm-define (server-credentials-update credentials credentials2)
  (if (null? credentials2) credentials
      (server-credential-update
       (server-credentials-update credentials (cdr credentials2))
       (car credentials2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Credential checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-password-authentified? pseudo passwd credentials)
  (with hiddens (filter server-credential-password?
			(if (list? credentials) credentials
			    (list credentials)))
    (if (null? hiddens) #f
	(with hidden (car hiddens)
	  (server-password-correct? passwd hidden)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test for strong passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-strong-credential? cred)
  (if (not (and (list-2? cred)
                (or (== (car cred) `password)
                    (== (car cred) `tls-password))))
    (begin
      (server-log-write `error "wrong credentials to check") #f)
      (if (server-strong-password? (second cred))
        #t
        (begin (server-log-write `error "password is not strong enough") #f))))

(tm-define (server-strong-credentials? credentials)
  (or (null? credentials)
      (and (server-strong-credential? (car credentials))
	   (server-strong-credentials? (cdr credentials)))))
