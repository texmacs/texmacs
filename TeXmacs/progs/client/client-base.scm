
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-base.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;                   2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-base)
  (:use (client client-authentication)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error and success widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(server-define-error-codes)

(tm-widget ((client-error-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (client-open-error s)
  (:interactive #t)
  (dialogue-window (client-error-widget s) noop "Error"))

(tm-widget ((client-success-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (client-open-success s)
  (:interactive #t)
  (dialogue-window (client-success-widget s) noop "Success"))

(tm-define (client-remote-then-cb server endpoint cb cb-err)
  (with wcb (lambda (ret) (if (list? ret) (cb ret) (cb-err ret)))
    (client-remote-eval* server endpoint wcb)))

(tm-define (client-remote-then server endpoint cb err-msg)
  (client-remote-then-cb
    server endpoint cb (lambda (ret) (client-open-error (string-append err-msg ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of call backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define call-back-dispatch-table (make-ahash-table))

(tm-define-macro (tm-call-back proto . body)
  (if (npair? proto) '(noop)
      (with (fun . args) proto
	`(begin
           (tm-define (,fun envelope ,@args)
             (catch #t
		    (lambda () ,@body)
                    (lambda (key . err-args)
                      (with msg (apply format-err key err-args)
                            (format #t "Client error in callback ~A: ~A\n"
                                    (symbol->string ',fun) msg)
                            (client-error envelope msg)))))
	   (ahash-set! call-back-dispatch-table ',fun ,fun)))))

(tm-define (client-eval envelope cmd)
  (when (debug-get "remote")
    (display* "client-eval " envelope ", " cmd "\n"))
  (cond ((and (pair? cmd) (ahash-ref call-back-dispatch-table (car cmd)))
         (with (name . args) cmd
           (with fun (ahash-ref call-back-dispatch-table name)
             (apply fun (cons envelope args)))))
        ((symbol? (car cmd))
         (with s (symbol->string (car cmd))
           (client-error envelope (string-append "invalid command '" s "'"))))
        (else (client-error envelope "invalid command"))))

(tm-define (client-return envelope ret-val)
  (with (server msg-id) envelope
    (client-send server `(server-remote-result ,msg-id ,ret-val))))

(tm-define (client-error envelope error-msg)
  (with (server msg-id) envelope
    (client-send server `(server-remote-error ,msg-id ,error-msg))))

(tm-call-back (local-eval cmd)
  (when #f ;; only set to #t for debugging purposes
    (with ret (eval cmd)
      (when (debug-get "remote")
        (display* "local-eval " cmd " -> " ret "\n"))
      (client-return envelope ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-server-active? (make-ahash-table))
(define client-serial 0)

(tm-define (active-servers)
  (ahash-set->list client-server-active?))

(tm-define (client-send server cmd)
  (client-write server (object->string* (list client-serial cmd)))
  (set! client-serial (+ client-serial 1)))

(tm-define (client-add server)
  (ahash-set! client-server-active? server #t)
  (with wait 1
    (delayed
      (:while (ahash-ref client-server-active? server))
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with msg (client-read server)
        (when (!= msg "")
          (with (msg-id msg-cmd) (string->object msg)
            (client-eval (list server msg-id) msg-cmd)
            (set! wait 1)))))))

(tm-define (client-remove server)
  (ahash-remove! client-server-active? server))

(tm-define (client-remove-notify server msg)
  (client-open-error msg)
  (client-remove server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-continuations (make-ahash-table))
(define client-error-handlers (make-ahash-table))

(tm-define (std-client-error msg)
  ;;(texmacs-error "client-remote-error" "remote error ~S" msg)
  (with t (if (string-ends? msg "\n") msg (string-append msg "\n"))
    (with s (if (string-starts? msg "Error: ") (substring t 7) t)
      (if (headless?)
	  (display-err* "Remote error: " s)
	  (debug-message "remote-error" s)))))

(tm-define (client-remote-eval server cmd cont . opt-err-handler)
  (when (debug-get "remote")
    (display* "client-remote-eval " (list server client-serial) ", " cmd "\n"))
  (with err-handler std-client-error
    (if (nnull? opt-err-handler) (set! err-handler (car opt-err-handler)))
    (ahash-set! client-continuations client-serial (list server cont))
    (ahash-set! client-error-handlers client-serial (list server err-handler))
    (client-send server cmd)))

(tm-define (client-remote-eval* server cmd cont)
  (client-remote-eval server cmd cont cont))

(tm-call-back (client-remote-result msg-id ret)
  (with server (car envelope)
    (and-with val (ahash-ref client-continuations msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server cont) val
        (when (== server orig-server)
          (cont ret))))))

(tm-call-back (client-remote-error msg-id err-msg)
  (with server (car envelope)
    (and-with val (ahash-ref client-error-handlers msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server err-handler) val
        (when (== server orig-server)
          (err-handler err-msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server names and ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-brackets s)
  (string-replace (string-replace s "[" "") "]" ""))

(define (sub-split-server-name-and-port s)
  (with v (string-decompose s ":")
    (with l (last v)
      (with p (string->number l)
	(if (integer? p)
	    (list (string-recompose (list-drop-right v 1) ":") l)
	    (list s "6561"))))))

(define (aux-split-server-name-and-port s)
  (with v (string-decompose s ":")
    (if (> (length v) 2)
	(if (string-contains? s "]")
	    (with p (sub-split-server-name-and-port s)
	      (list (remove-brackets (first p)) (second p)))
	    (list s "6561"))
	(sub-split-server-name-and-port s))))

(define (split-server-name-and-port s)
  (with l (aux-split-server-name-and-port s)
    ;(display* "split-server-name-and-port " s " --> " l "\n")
    l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-active-connections (make-ahash-table))

(tm-define (add-active-connection server server-name port pseudo)
  (ahash-set! client-active-connections server (list server-name port pseudo))
  (ahash-set! client-active-connections (list server-name port) server))

(define (remove-active-connection server server-name port pseudo)
  (ahash-remove! client-active-connections server)
  (ahash-remove! client-active-connections (list server-name port)))

(tm-define (client-find-server-by-name-and-port server-name port)
  ;(display* server-name " -> " (ahash-ref client-active-connections
  ;					  server-name) "\n")
  (ahash-ref client-active-connections (list server-name port)))

(tm-define (client-find-server server-name-port)
  (with (server-name port) (split-server-name-and-port server-name-port)
    (client-find-server-by-name-and-port server-name port)))

(tm-define (client-find-server-name server)
  (and-with p (ahash-ref client-active-connections server) (first p)))

(tm-define (client-find-server-port server)
  (and-with p (ahash-ref client-active-connections server) (second p)))

(tm-define (client-find-server-pseudo server)
  (and-with p (ahash-ref client-active-connections server) (third p)))

(tm-define (client-active-servers)
  (list-filter (active-servers) client-find-server-name))

(tm-define (client-active-admin-servers)
  (list-filter (active-servers)
               (lambda (s) (and (client-find-server-name s)
                                (server-connection-admin? s)))))

(tm-define (client-notify-account server-name port pseudo authentications admin?)
  (with-database (user-database "remote")
      (let* ((l `(("type" "account")
                  ("server" ,server-name)
                  ("port" ,port)
                  ("pseudo" ,pseudo)))
             (ids (db-search l)))
        (if (null? ids)
          (db-create-entry (append l
                                   `((authentications . ,authentications))
                                   `((admin . (,admin?)))))
          (let* ((id (car ids))
                 (auth-old (client-normalize-authentications
                             (db-get-field id "authentications")))
                 (auth-new (client-merge-authentications
                             authentications auth-old)))
            ;(display* "updating entry, auth: " auth-new ", admin: " admin?  "\n")
            (db-set-field id "authentications" auth-new)
            (db-set-field id "admin" (list admin?)))))))

(tm-define (client-accounts)
  (with-database (user-database "remote")
    (let* ((ids (db-search `(("type" "account"))))
	   (get (lambda (id)
		  (list (db-get-field-first id "server" "")
			(db-get-field-first id "port" "6561")
			(db-get-field-first id "pseudo" "")
			(client-normalize-authentications
			 (db-get-field id "authentications"))))))
      (map get ids))))

(tm-define (client-remove-account server-name port pseudo)
  (with-database (user-database "remote")
    (with ids (db-search `(("type" "account")
                           ("server" ,server-name)
                           ("port" ,port)
                           ("pseudo" ,pseudo)))
      (when (nnull? ids)
        (db-remove-entry (car ids))))
    (when (== port "6561") ; backward compatibility
      (with ids (db-search `(("type" "account")
			     ("server" ,server-name)
			     ("pseudo" ,pseudo)))
	(when (nnull? ids)
	  (db-remove-entry (car ids)))))))

(tm-define (client-account-admin? server-name port pseudo)
  (with-database (user-database "remote")
    (with ids (db-search `(("type" "account")
                           ("server" ,server-name)
                           ("port" ,port)
                           ("pseudo" ,pseudo)
                           ("admin" "#t")))
          (nnull? ids))))

(tm-define (server-connection-admin? server)
  (and-with p (ahash-ref client-active-connections server) 
            (client-account-admin? (first p) (second p) (third p))))

;; New account
(tm-define (client-new-account server infos cb-pending cb-done cb-err)
  (let ((server-name (ahash-ref infos "server-name"))
        (port (ahash-ref infos "port"))
        (pseudo (ahash-ref infos "pseudo"))
        (name (ahash-ref infos "name"))
        (creds (map client-hide-credential (ahash-ref infos "creds")))
        (email (ahash-ref infos "email"))
        (agreed (ahash-ref infos "agreed")))
  ;(display* "creating new account server-name: " server-name ", port: " port ", protocol: " protocol ", pseudo: " pseudo ", name: " name ", email: " email "\n")
  (client-remote-eval*
    server `(new-account ,pseudo ,name ,creds ,email ,agreed)
    (lambda (msg)
      (set-message msg "Creating new remote account")
      (cond
        ((== msg "done")    (cb-done server server-name port pseudo creds))
        ((== msg "pending") (cb-pending server server-name port pseudo creds))
        ((== msg "user already exists")
         (cb-err (string-append "Remote account creation failed: user '"
                                pseudo "' already exists")))
        (else (cb-err (string-append "Remote account creation failed: "
                                     msg))))))))

(tm-define (client-remove-accounts server pseudos cb)
  (client-remote-then server `(remote-remove-accounts ,pseudos) cb
                      "Cannot remove remote account(s): "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account informations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-get-account-then server user cb)
  (client-remote-then server `(remote-get-account ,user) cb
                      "Cannot retrieve remote account information: "))

(tm-define (client-get-accounts-then server limit offset cb)
  (client-remote-then server `(remote-get-accounts ,limit ,offset) cb
                      "Cannot retrieve user accounts preferences: "))

(tm-define (client-set-account server infos)
  (with cb (lambda (ret)
              (if (== ret "done")
                  (client-open-success "Remote account information updated")
                  (client-open-error
		   (string-append
		    "Cannot set remote account information: " ret))))
    (client-remote-eval* server `(remote-set-account #f ,infos) cb)))

(tm-define (client-admin-set-account server infos user)
  (with cb (lambda (ret)
              (if (== ret "done")
                  (client-open-success "Remote account information updated")
                  (client-open-error
		   (string-append
		    "Cannot set remote account information: " ret))))
    (client-remote-eval* server `(remote-set-account ,user ,infos) cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (client-start-errno->string e)
  (cond ((== e tm_net_invalid_host) "invalid host name")
	((== e tm_net_invalid_port) "invalid port number")
	((== e tm_net_internal_error) "internal error")
	((== e tm_net_contact_dead) "could not start contact")
	((== e tm_net_wrong_protocol) "wrong protocol")
	((== e tm_net_no_gnutls) "missing GnuTLS")
	(else "connection failed")))

;; Legacy password authentication
(tm-define (legacy-password-client-login-then
	    server-name port pseudo passwd cb)
  (with server (legacy-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
	  (client-remote-eval* server
			       `(remote-login ,pseudo ,passwd)
                               (lambda (ret) (cb server ret)))))))

;; TLS password authentication
(tm-define (tls-password-client-login-then
	    server-name port pseudo passwd cb)
  (with server (tls-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
	  (client-remote-eval* server
			       `(remote-login ,pseudo ,passwd)
                               (lambda (ret) (cb server ret)))))))

;; Dispatch
(tm-define (client-login-then server-name port pseudo credential cb)
  (cond
    ((and (list-2? credential) (== (car credential) `legacy-password))
     (legacy-password-client-login-then
      server-name port pseudo (second credential) cb))
    ((and (list-2? credential) (== (car credential) `tls-password))
     (tls-password-client-login-then
      server-name port pseudo (second credential) cb))
    ((or (nlist? credential) (null? credential))
     (cb "unknown credential type"))
    (else (cb (string-append "Unsupported authentication type "
			     (object->string (car credential)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login with code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Legacy code authentication
(tm-define (legacy-client-login-code-then
	    server-name port protocol pseudo code cb)
  (with server (legacy-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
	  (client-remote-eval* server
			       `(remote-login-code ,pseudo ,code)
                               (lambda (ret) (cb server ret)))))))

;; TLS code authentication
(tm-define (tls-client-login-code-then
	    server-name port protocol pseudo code cb)
  (with server (tls-anonymous-client-start server-name port)
    (if (< server 0) (cb server (client-start-errno->string server))
        (begin
	  (client-remote-eval* server
			       `(remote-login-code ,pseudo ,code)
                               (lambda (ret) (cb server ret)))))))

;; Dispatch
(tm-define (client-login-code-then server-name port protocol pseudo code cb)
  (cond
    ((== protocol `legacy)
     (legacy-client-login-code-then
      server-name port protocol pseudo code cb))
    ((== protocol `tls)
     (tls-client-login-code-then
      server-name port protocol pseudo code cb))
    (else (cb (string-append "Unsupported authentication type "
			     (object->string protocol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-logout server)
  (and-with server-con (ahash-ref client-active-connections server)
    (with (server-name server-port server-pseudo) server-con
      (with cb (lambda (ret)
		 (if (!= ret "bye")
		     (std-client-error "Logout failed"))
                 (remove-active-connection server server-name server-port server-pseudo)
		 (for (sv (active-servers))
		   (and-with sv-conn (ahash-ref client-active-connections sv)
		     (with (sv-name sv-pseudo) sv-conn
		       (ahash-set! client-active-connections sv-name sv))))
		 (set! remote-client-list (client-active-servers))
		 (client-stop server))
	(client-remote-eval* server `(remote-logout) cb)))))
