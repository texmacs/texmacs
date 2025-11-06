
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-authentication.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-authentication)
  (:use (database db-users)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contact timeout in ms, default value is 10s
(define-preferences
  ("client contact timeout" "10000"
   (lambda (var val) (noop))))

(tm-define (client-get-contact-timeout)
  (string->number (get-preference "client contact timeout")))

(tm-define (client-set-contact-timeout val)
  (:interactive)
  (:argument val "Contact timeout in ms")
  (when (or (number? val) (string-number? val))
    (set-preference "client contact timeout" val)))

;; Connection timeout in seconds
(define-preferences
  ("client connection timeout" "100"
   (lambda (var val) (noop))))

(tm-define (client-get-connection-timeout)
  (string->number (get-preference "client connection timeout")))

(tm-define (client-set-connection-timeout val)
  (:interactive)
  (:argument val "Connection timeout in seconds")
  (when (or (number? val) (string-number? val))
    (set-preference "client connection timeout" val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Supported authentications: legacy-password, tls-password

(define (insert-authentications e l)
  (cons e (filter (lambda (x) (not (equal? x e))) l)))
  
(tm-define (client-merge-authentications l1 l2)
  (if (null? l1) l2
      (insert-authentications (car l1)
			      (client-merge-authentications (cdr l1) l2))))

(tm-define (client-hide-credential cred)
  (cond
    ((string? cred) `(legacy-password ,cred)) ;; For compatibility
    ((and (list-2? cred) (== (car cred) `password))
     `(password ,(second cred)))
    ((and (list-2? cred) (== (car cred) `legacy-password))
     `(legacy-password ,(second cred)))
    ((and (list-2? cred) (== (car cred) `tls-password))
     `(tls-password ,(second cred)))
    (else `())))

(tm-define (client-hide-credentials credentials)
  (map client-hide-credential credentials))

(define (supported-authentication? a)
  (in? a `(legacy-password tls-password)))

(tm-define (client-normalize-authentications l)
  (list-remove-duplicates
   (filter supported-authentication?
	   (map (lambda (x) (if (string? x) (string->object x) x)) l))))

(tm-define (client-authentication->string a)
  (cond ((== a `legacy-password) "Password via legacy server")
	((== a `tls-password) "Password via TLS")
        (else "unknown authentication")))

(tm-define (client-string->authentication s)
  (cond ((== s "Password via legacy server") `legacy-password)
	((== s "Password via TLS") `tls-password)
        (else `unknown)))

(tm-define (client-authentication->protocol a)
  (cond ((== a `legacy-password) `legacy)
	((== a `tls-password) `tls)
        (else `unknown)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting anonymous clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; legacy-client-start is wrapped to ensure that enter-secure-mode is
;; activated at startup.

(define (port-string->integer port)
  (with p (string->number port)
    (if (integer? p) p 6561)))

(tm-define (legacy-anonymous-client-start server-name port)
  (with server (legacy-client-start
		server-name (port-string->integer port))
    (if (> server 0)
	(begin
	  (enter-secure-mode server)
	  server)
	server)))

(tm-define (tls-anonymous-client-start server-name port)
  (tls-client-start server-name (port-string->integer port) `((anonymous))))

(tm-define (anonymous-client-start server-name port protocol)
  (cond ((== protocol `legacy)
	 (legacy-anonymous-client-start server-name port))
	((== protocol `tls)
	 (tls-anonymous-client-start server-name port))
	(else -70)))
