
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-widgets.scm
;; DESCRIPTION : widgets for remote clients
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;                   2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-widgets)
  (:use (client client-tmfs)
        (client client-sync)
        (client client-chat)
        (client client-live)))

(tm-define (tm-servers)
  ;(list "server.texmacs.org" "texmacs.math.unc.edu" "localhost"))
  (list "localhost")) ;; todo, for testing purpose

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (client-preferences-widget quit)
  (padded
  (aligned
   (item
    (text "Contact timeout in milliseconds:") (hlist //
    (input (when answer (client-set-contact-timeout answer))
          "string"
          (list (number->string (client-get-contact-timeout)))
          "7em")))
   (item
    (text "Connection timeout in seconds:") (hlist //
    (input (when answer (client-set-connection-timeout answer))
          "string"
          (list (number->string (client-get-connection-timeout)))
          "7em"))))))
;; todo << prevent from setting timeouts to 0

(tm-define (open-client-preferences)
  (dialogue-window client-preferences-widget noop
                   "Client preferences"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((save-license-as doc) f)
  (with s (convert doc "texmacs-stree" "texmacs-document")
    (string-save s f)))

(define (client-server-license server-preferences)
  (cond ((null? server-preferences) #f)
        ((== (first (car server-preferences)) "server license")
         (second (car server-preferences)))
        (else (client-server-license (cdr server-preferences)))))

(tm-widget ((client-public-preferences-widget prefs license) quit)
  (padded
    (for (x prefs)
      (if (and (list? x) (== (length x) 2))
          (text (string-append (first x) ": " (second x)))))
    (if license
        (vlist
          ======
          (resize "410px" "200px"
            (scrollable
              (texmacs-output `(with "bg-color" "#fff"
                                 ,(tmfile-extract license 'body))
                              `(style ,(tmfile-extract license 'style)))))
          ======
          (bottom-buttons
            >>
            ("Save license"
             (choose-file (save-license-as license)
                          "Save license" "texmacs")) // // //
            ("Close" (quit)))))))

(tm-define (open-public-preferences server)
  (with cb (lambda (ret)
    (let* ((license (client-server-license ret))
           (others (filter (lambda (x) (!= (car x) "server license")) ret)))
      (dialogue-window (client-public-preferences-widget others license)
                       noop "Remote server preferences")))
    (client-public-preferences-then server cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confirm pending account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-confirm-pending-account
             server server-name port pseudo credentials code)
  (with cb (lambda (ret)
             (client-stop server)
             (if (== ret "done")
               (begin (client-notify-account server-name port pseudo
                                             (map car credentials) #f)
                      (client-open-success "account created"))
               (client-open-error
                 (string-append "account confirmation failed, " ret))))
        (client-remote-eval*
          server `(confirm-pending-account ,pseudo ,code) cb)))

(tm-widget ((remote-confirm-pending-account-widget
	     server server-name port pseudo credentials) quit)
  (padded
    (form "remote-confirm-pending-account"
      ======
      (centered (text "A confirmation email has been sent to your address."))
      (centered (text "Please check your inbox and spam folder for the email."))
      (aligned
        (item (text "Enter the confirmation code:")
          (form-input "code" "string"
                      (list "") "100px")))
      ======
      (bottom-buttons
        >>
        ("Cancel" (quit))
	// //
	("Ok"
	 (client-confirm-pending-account
	  server server-name port pseudo credentials (first (form-values)))
	 (quit))))))

(tm-define (open-remote-confirm-pending-account
             server server-name port pseudo creds)
  (dialogue-window (remote-confirm-pending-account-widget
		    server server-name port pseudo creds)
		   noop "Confirm pending account"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-incomplete-field t pw? key?)
  (cond ((== (ahash-ref t "server") "") "Server")
	((== (ahash-ref t "pseudo") "") "Pseudo")
	((== (ahash-ref t "name") "") "Full name")
	((== (ahash-ref t "email") "") "Email")
	((and pw? (== (ahash-ref t "password") "")) "Password")
	((and pw? (== (ahash-ref t "repeat") "")) "Repeat")
	(else #f)))

(define (client-matched-authentications server-preferences)
  (append
   (if (and (supports-gnutls?)
            (in? `("tls-server" "on") server-preferences)
            (in? `("tls-server authentication anonymous" "on")
                 server-preferences))
       (list "Password via TLS") '())
   (if (in? `("server service login" "on") server-preferences)
       (list "Password") '())))

(define (on-account-creation-done server server-name port pseudo credentials)
  ; this is called from client account creation, always set admin to #f
  (client-notify-account server-name port pseudo
                         (map car credentials) #f)
  (with-wallet
    (for (credential credentials)
         (wallet-set (list "remote" server-name port
                           pseudo (car credential))
                     credential)))
  (client-open-success "Remote account creation successful")
  (client-stop server))

(tm-widget (remote-account-server-widget cb)
  (let* ((protocol `tls))
  (padded
    (form "account-server-info"
      ======
      (aligned
      (item (text "Server:")
             (form-input "server" "string"
                         (list "cloud.texmacs.org" "localhost") "300px"))
      (item (text "Port:")
             (form-input "port" "string"
                         (list "6561") "300px"))
      (item (text "Protocol:")
	(enum (set! protocol (string->object (string-downcase answer)))
	      (list "TLS" "legacy") "TLS" "280px")))
      ======
      (bottom-buttons >>
        ("Continue"
	 (and-with server-name (first (form-values))
	   (and-with port (second (form-values))
	     (with server (anonymous-client-start
			   server-name port protocol)
	       (if (< server 0)
		   (client-open-error "Connection to server failed")
		   (client-public-preferences-then
		    server
		    (lambda (prefs)
		      (cb server server-name port prefs)))))))))))))

(tm-widget ((remote-account-widget server server-name port server-preferences cb-pending cb-done cb-err)
	    quit)
  (let* ((dummy (begin
                  (form-named-set "account-info" "password" "")
                  (form-named-set "account-info" "repeat" "")))
         (matched-authentications
          (client-matched-authentications server-preferences))
         (auth (if (null? matched-authentications)
                   "" (car matched-authentications)))
         (use-password? #f)
         (use-key? #f)
         (license (client-server-license server-preferences))
         (agreed? (not license)))
    (padded
      (form "account-info"
        (hlist >> (aligned
          (item (text "Server:")
            (text server-name))
          (item (text "Port:")
            (text port))
          (item (text "Pseudo:")
            (form-input "pseudo" "string"
                        (list (get-user-info "pseudo")) "280px"))
          (item (text "Full name:")
            (form-input "name" "string"
                        (list (get-user-info "name")) "280px"))
          (item (text "Email:")
            (form-input "email" "string"
                        (list (get-user-info "email")) "280px"))))
        ===
        (hlist >> (aligned (item (text "Authentication method:")
          (enum (begin (set! auth answer)
                       (refresh-now "auth-info"))
                matched-authentications auth "280px"))))
        ===
        (refreshable "auth-info"
            (hlist >> (aligned
              (item (text "Password:")
                (form-input "password" "password"
                            (list (fourth (form-values))) "280px"))
              (item (text "Repeat password:")
                (form-input "repeat" "password"
                            (list (fifth (form-values))) "280px")))))
        (if license
            ======
            (hlist >> (resize "410px" "200px"
              (scrollable
                (texmacs-output `(with "bg-color" "#fff"
                                   ,(tmfile-extract license 'body))
                                `(style ,(tmfile-extract license 'style))))))
            ===
            (hlist >>
              (toggle (set! agreed? answer) agreed?) // // //
              (vlist (text "I declare having read and agreed with")
                     (text "the above license agreement")) >>))
        ======
        (bottom-buttons
          >>
          ("Cancel" (quit)) // // //
          (if license
              ("Save license"
               (choose-file (save-license-as license)
                            "Save license" "texmacs")) // // //)
          ("Proceed"
           (with t (make-ahash-table)
             (for-each (cut ahash-set! t <> <>) (form-fields) (form-values))
             (ahash-set! t "server-name" server-name)
             (ahash-set! t "port" port)
	     (set! use-password? #t)
             (cond ((not (or use-password? use-key?))
                    (client-open-error
                     "Please enable at least one authentification method"))
                   ((and use-password? (!= (ahash-ref t "password")
                                           (ahash-ref t "repeat")))
                    (client-open-error "Passwords do not match"))
                   ((not agreed?)
                    (client-open-error
                     (string-append "You must agree with the license "
                                    "in order to proceed")))
                   ((first-incomplete-field t use-password? use-key?) =>
                    (lambda (s)
                      (client-open-error (string-append "Missing '" s "'"))))
                   (else
                     (when (== auth "Password")
                       (ahash-set! t "creds"
                                   `((password ,(ahash-ref t "password")))))
                     (when (== auth "Password via TLS")
                       (ahash-set! t "creds"
                                   `((tls-password ,(ahash-ref t "password")))))
                     (ahash-set! t "agreed" (if agreed? "yes" "no"))
                     (with uid (pseudo->user (ahash-ref t "pseudo"))
                       (when (== uid (get-default-user))
                         (when (== (get-user-info "name") "")
                           (set-user-info "name" (ahash-ref t "name")))
                         (when (== (get-user-info "email") "")
                           (set-user-info "email" (ahash-ref t "email")))))
                       (client-new-account server t
                                           cb-pending
                                           cb-done
                                           cb-err)
                       (quit))))))))))

(tm-define (open-remote-account-creator)
  (:interactive #t)
  (dialogue-window remote-account-server-widget
    (lambda (server server-name port prefs)
      (if (null? (client-matched-authentications prefs))
          (client-open-error "No authentication methods")
          (dialogue-window
           (remote-account-widget server server-name port prefs
                                  open-remote-confirm-pending-account
                                  on-account-creation-done
                                  (lambda (msg)
                                    (client-open-error msg)
                                    (client-stop server)))
           noop
           "Create remote account")))
    "Create remote account"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pending login widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-pending-login-home
	    server server-name port pseudo credential code)
  (with cb (lambda (ret)
	     (client-stop server)
	     (if (== ret "done")
               (client-notify-account server-name port pseudo
                                      (map car credentials) #f)
               (client-login-home server-name port pseudo credential noop)
               (client-open-error
                 (string-append "account confirmation failed: " ret))))
    (client-remote-eval*
     server `(confirm-pending-account ,pseudo ,code) cb)))

(tm-widget ((remote-pending-login-widget
	     server server-name port pseudo credential) quit)
  (padded
    (form "pending-login-info"
      ======
      (aligned
        (item (text "Confirmation code:")
          (form-input "code" "string"
                      (list "") "100px")))
      ======
      (bottom-buttons
        >>
        ("Cancel"
	 (when server (client-logout server))
	 (quit))
	// //
	("Ok"
	 (client-pending-login-home
	  server server-name port pseudo credential (first (form-values)))
	 (quit))))))

(tm-define (open-remote-pending-login
	    server server-name port pseudo credential)
  (dialogue-window (remote-pending-login-widget
		    server server-name port pseudo credential)
		   noop "Remote pending login"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-login-home server-name port pseudo credential cb-done)
  (with authentication (car credential)
    (client-login-then server-name port pseudo credential
      (lambda (server ret)
        (cond ((== ret "ready")
               (begin
                 (add-active-connection server server-name port pseudo)
                 (set! remote-client-list (client-active-servers))
                 (client-get-account-then
                   server #f
                   (lambda (ret)
                     (client-notify-account
                       server-name port pseudo `(,authentication)
                       (car (ahash-ref (list->ahash-table ret) "admin")))
                     (with-wallet
                       (wallet-set (list "remote" server-name port
                                         pseudo authentication)
                                   credential))
                     (load-buffer (remote-home-directory server))
                     (cb-done)))))
              ((== ret "pending")
               (add-active-connection server server-name port pseudo)
               (set! remote-client-list (client-active-servers))
               (open-remote-pending-login
                 server server-name port pseudo credential))
              (else
                (when server (client-logout server))
                (client-open-error
                  (string-append "Remote login error, " ret))))))))

(tm-widget ((remote-login-widget server-name port pseudo authentication)
	    quit)
  (let* ((auth authentication))
  (padded
    (form "login-info"
      ======
      (aligned
        (item (text "Server:")
          (form-input "server" "string"
                      (if (== server-name "")
			  (list "cloud.texmacs.org" "localhost")
			  (list server-name)) "300px"))
        (item (text "Port:")
          (form-input "port" "string"
                      (list port) "300px"))
        (item (text "Pseudo:")
          (form-input "pseudo" "string"
                      (list pseudo) "300px"))
       (item (text "Authentication:")
          (enum (set! auth (client-string->authentication answer))
                (append
                  (if (supports-gnutls?) (list "Password via TLS") '())
                  (list "Password via legacy server"))
            (client-authentication->string auth) "280px"))
        (item (text "Password:")
          (form-input "password" "password"
                      (list "") "300px")))
      ======
      (bottom-buttons
        >>
	("Reset credentials"
	 (open-remote-notify-reset-credentials
	  (first (form-values))
	  (second (form-values))
	  (third (form-values))
	  (client-authentication->protocol auth))
	 (quit)) // //
        ("Cancel" (quit)) // //
        ("Login" (cond ((in? auth `(legacy-password tls-password))
		     (client-login-home
		      (first (form-values))
		      (second (form-values))
		      (third (form-values))
		      `(,auth ,(fourth (form-values)))
                      (lambda () (quit))))
		    (else (client-open-error "Unexpected authentication type")))))))))
;; toto << add (quit)

(tm-define (open-remote-login server-name port pseudo authentications)
  (if (or (nlist? authentications) (null? authentications))
      (dialogue-window (remote-login-widget server-name port pseudo `tls-password)
		       noop "Remote login")
      (with authentication (car authentications)
	(with-wallet
	  (with credential (wallet-get (list "remote" server-name port
					     pseudo authentication))
	    (if credential
		(client-login-home server-name port pseudo credential (lambda () (quit)))
		(dialogue-window (remote-login-widget server-name port pseudo
						      authentication)
				 noop "Remote login")))))))

(define (line-feed-universal s)
  (string-replace s "\n" "<#0A>"))

(tm-define (trust-certificate-interactive crt-desc crt-pem)
  (:interactive #t)
  (with msg (line-feed-universal
              (string-append "The server certificate issuer is unknown\n"
                             "Do you still want to trust it ?\n\n" crt-desc))
    (user-confirm msg #f
      (lambda (answ) (when answ (trust-certificate crt-pem))))))

(tm-define (disable-certificate-time-checks-interactive crt-desc)
  (:interactive #t)
  (with msg (line-feed-universal
              (string-append "The certificate presented is either expired or"
                             " not yet active.  By disabling time verification"
                             " checks, you will allow all certificates"
                             " to bypass time validations, which may expose"
                             " your connection to security risks.\n\n"
                             "Do you want to proceed with disabling time"
                             " verification for all trusted certificates?\n\n"
                             crt-desc))
    (user-confirm msg #f
      (lambda (answ) (when answ (disable-certificate-time-checks))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((client-edit-account-widget server uid info admin?) quit)
  (let* ((dummy (form-named-set "client-edit-account-form" "password" ""))
	 (w (list->ahash-table info))
         (get (lambda (key) (with x (ahash-ref w key)
                              (and (list? x) (nnull? x) (car x)))))
         (pseudo (get "pseudo"))
         (name (get "name"))
         (email (get "email"))
         (admin (get "admin"))
         (authentications (get "authentications"))
         (credentials `())
         (edit-password? #f)
         (password (in? `password authentications)))
    (padded
      (refreshable "client-edit-account-refreshable"
	(form "client-edit-account-form"
	  (aligned
	    (item (text "Pseudo:") (text pseudo))
	    (item (text "Full name:")
	      (form-input "name" "string" (list name) "300px"))
	    (item (text "Email:")
	      (form-input "email" "string" (list email) "300px"))
	    (item (text "Administrator:") (text (if admin "yes" "no")))
	    (item (text "Password:")
	      (hlist
		(if edit-password?
		    (form-input "password" "password"
				(list (third (form-values))) "300px"))
		(explicit-buttons
		  (if (and (not edit-password?) (not password))
		      ("Add"
		       (set! edit-password? #t)
		       (refresh-now "client-edit-account-refreshable"))
		      // // //)
		  (if (and (not edit-password?) password)
		      ("Delete"
		       (set! password #f)
		       (refresh-now "client-edit-account-refreshable"))
		      // // //
		      ("Change"
		       (set! edit-password? #t)
		       (refresh-now "client-edit-account-refreshable"))))
		>> )))
	  === ===
	  (hlist
	    >>
	    (explicit-buttons
	      ("Cancel"
	       (quit))
	      // // //
	      ("Apply"
	       (with t (make-ahash-table)
		 (for-each (cut ahash-set! t <> <>)
			   (form-fields) (form-values))
		 (set! name (ahash-ref t "name"))
		 (set! email (or (ahash-ref t "email") ""))
		 (if (== name "")
		     (client-open-error "Missing 'name'")
		     (if (== email "") (client-open-error "Missing 'email'")))
		 (when (and (!= name "") (!= email ""))
		   (when edit-password?
		     (set! password
			   (client-hide-credential
			    `(password ,(or (ahash-ref t "password") "")))))
		   (when (== password #f)
		     (set! credentials (cons `(password) credentials)))
		   (when (func? password `password)
		     (set! credentials (cons password credentials)))
		   (if admin? 
                     (client-admin-set-account server
                       `(("name" ,name)
                         ("email" ,email)
                         ("credentials" ,credentials))
                       uid)
                     (client-set-account server
                      `(("name" ,name)
                        ("email" ,email)
                        ("credentials" ,credentials))))
		   (quit)))))))))))

(tm-define (open-account-editor server)
  (with cb (lambda (ret)
    (dialogue-window (client-edit-account-widget server #f ret #f)
		     noop "Remote account information"))
    (client-get-account-then server #f cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote Admin User Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-admin-account-editor server uid)
  (with cb
    (lambda (ret)
      (dialogue-window
        (client-edit-account-widget server uid ret #t)
        noop "Edit server account"))
    (client-get-account-then server uid cb)))

(define (client-get-accounts server limit offset)
  (client-get-accounts-then 
    server limit offset
    (lambda (ret)
      (dialogue-window
        (admin-remote-accounts-widget server ret) noop "Server accounts"))))

(tm-widget ((admin-remote-accounts-widget server users) quit)
  (let ((uid "")
        (cb-done (lambda (server server-name port pseudo creds)
                   (client-open-success
                     "Remote account creation successful"))))
    (padded
      (resize '("450px" "450px" "9999px") '("250px" "250px" "9999px")
	(refreshable "remote-accounts-choice-refreshable"
	  (scrollable
	    (choice (begin
		      (set! uid answer)
		      (refresh-now "remote-accounts-buttons-refreshable"))   
                    users
		    uid))))
      ===
      (refreshable "remote-accounts-buttons-refreshable"
	(hlist >> (explicit-buttons
		    ("Cancel" (quit))  // // //       
		    (when (!= uid "")
		      ("Edit"
		       (open-admin-account-editor server uid)) // // //
		       ("Delete"
		       (server-open-error "not implemented")) // // //)
		    ("Add"
                     (client-public-preferences-then
                       server
                       (lambda (prefs)
                         (dialogue-window
                           (remote-account-widget
                             server
                             (client-find-server-name server)
                             (client-find-server-port server)
                             prefs
                             cb-done
                             cb-done
                             (lambda (msg)
                               (client-open-error msg)))
                           (lambda ()
                             (refresh-now "remote-accounts-choice-refreshable"))
                           "Create server account"))))))))))

(tm-define (open-admin-accounts-editor server)
  (:interactive #t)
  (client-get-accounts server 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset credentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((remote-notify-reset-credentials-widget
	     server-name port pseudo protocol) quit)
  (padded
    (form "account-server-info"
      ======
      (aligned
      (item (text "Server:")
             (form-input "server" "string"
               (list server-name "cloud.texmacs.org" "localhost") "300px"))
      (item (text "Port:")
             (form-input "port" "string"
                         (list port) "300px"))
      (item (text "Pseudo:")
	 (form-input "pseudo" "string"
		     (list pseudo) "300px"))
      (item (text "Protocol:")
	(enum (set! protocol (string->object (string-downcase answer)))
	      (list "TLS" "legacy") (string-upcase (object->string protocol)) "280px")))
      ======
      (bottom-buttons >>
	("Cancel" (quit)) >>
        ("Continue"
	 (let* ((server-name (first (form-values)))
		(port (second (form-values)))
		(pseudo (third (form-values)))
		(server (anonymous-client-start server-name port protocol)))
	       (if (< server 0)
		   (client-open-error "Connection to server failed")
		   (client-remote-eval*
		    server `(remote-reset-credentials ,pseudo)
		    (lambda (ret)
		      (if (== ret "done")
			  ;(client-open-success "an email has been sent to you")
			  (open-remote-login-code server-name port pseudo protocol)
			  (client-open-error ret))))))
	 (quit))))))

(tm-define (open-remote-notify-reset-credentials
	    server-name port pseudo protocol)
  (when (not protocol) (set! protocol `tls))
  (dialogue-window (remote-notify-reset-credentials-widget
		    server-name port pseudo protocol)
		   noop "Notify forgotten credentials"))

(tm-define (client-login-home-code server-name port pseudo protocol code)
  (client-login-code-then server-name port protocol pseudo code
     (lambda (server ret)
       (cond ((== ret "ready")
                (add-active-connection server server-name port pseudo)
                (set! remote-client-list (client-active-servers))
                (load-buffer (remote-home-directory server))
                (open-account-editor server))
             (else
               (when server (client-logout server))
               (client-open-error
                 (string-append "Remote login error, " ret)))))))

(tm-widget ((remote-login-code-widget
	     server-name port pseudo protocol) quit)
  (padded
    (form "remote-login-code-info"
      ======
      (aligned
      (item (text "Server:")
             (form-input "server" "string"
                         (list server-name) "300px"))
      (item (text "Port:")
             (form-input "port" "string"
                         (list port) "300px"))
      (item (text "Pseudo:")
	 (form-input "pseudo" "string"
		     (list pseudo) "300px"))
      (item (text "Protocol:")
	(enum (set! protocol (string->object (string-downcase answer)))
	      (list "TLS" "legacy") (string-upcase (object->string protocol)) "280px"))
      (item (text "Code:")
	 (form-input "code" "string"
		     (list "") "300px")))
      ======
      (bottom-buttons >>
	("Cancel" (quit)) >>
        ("Continue"
	 (let* ((server-name (first (form-values)))
		(port (second (form-values)))
		(pseudo (third (form-values)))
		(code (fourth (form-values))))
	   (client-login-home-code
	    server-name port pseudo protocol code)))))))

(tm-define (open-remote-login-code
	    server-name port pseudo protocol)
  (when (not protocol) (set! protocol `tls))
  (dialogue-window (remote-login-code-widget
		    server-name port pseudo protocol)
		   noop "Login with confirmation code"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-ancestors dir)
  (let* ((parent (remote-parent dir))
         (this (url->string (url-tail dir)))
         (last (cons this dir)))
    (if (and (not (remote-home-directory? dir))
	     (not (remote-root-directory? dir)))
        (rcons (get-ancestors parent) last)
        (list last))))

(define (remote-relative dir name dir?)
  (if (remote-root-directory? dir)
      (let* ((root (remote-file-name dir))
             (protocol (if dir? "tmfs://remote-dir/" "tmfs://remote-file/"))
             (full (string-append protocol root)))
        (url-append full name))
      (remote-relative (remote-parent dir) name dir?)))

(tm-widget ((remote-file-browser server initial type) quit)
  (let* ((save-flag? (or (func? type :save-file 1)
                         (func? type :save-directory 1)))
         (dir-flag? (or (== type :directory)
                        (func? type :save-directory 1)))
         (dir (if (and save-flag? (not (remote-home-directory? initial)))
		  (remote-parent initial) initial))
         (file (cond ((not save-flag?) #f)
		     ((remote-home-directory? initial) "")
		     (else (url->string (url-tail initial)))))
         (entries (list))
         (select-dir
          (lambda (d)
            (and-with name (remote-file-name d)
              ;;(display* "old dir= " dir "\n")
              ;;(display* `(remote-dir-load ,name) "\n")
              (client-remote-eval server `(remote-dir-load ,name)
                (lambda (new-entries)
                  ;;(display* "Got " new-entries "\n")
                  (set! dir d)
                  (set! entries new-entries)
                  ;;(display* "new dir= " dir "\n")
                  (refresh-now "remote-file-browser"))
                (lambda (err)
                  ;;(display* "Got " err "\n")
                  ;;(display* "new dir= " dir "\n")
                  (set-message err "remote directory"))))))
         (select-entry
          (lambda (e)
            (with (full-name dir? props) (assoc-ref entries e)
              (with name (remote-relative dir full-name dir?)
                (cond (dir? (select-dir name))
                      (save-flag?
                       (set! file (url->string (url-tail name)))
                       (refresh-now "remote-save-as"))
                      (else (quit name)))))))
         (list-entry?
          (lambda (e)
            (with (short-name full-name dir? props) e
              (if dir-flag? dir? #t))))
         (dummy (select-dir dir)))
    (padded
      (refreshable "remote-file-browser"
        (hlist
          (explicit-buttons
            (for (a (get-ancestors dir))
              ((eval (car a)) (select-dir (cdr a))) //)
            >>))
        ===
        (resize "600px" "400px"
          (choice (select-entry answer)
                  (map car (list-filter entries list-entry?))
                  ""))
        (assuming save-flag?
          ===
          (hlist
            (text (cadr type)) //
            (refreshable "remote-save-as"
              (input (when answer (quit (url-append dir answer)))
                     "string" (list file) "1w"))
            // // //
            (explicit-buttons
              ("Ok" (quit (url-append dir file)))))    )
        (assuming (and dir-flag? (not save-flag?))
          (bottom-buttons
            >>
            ("Ok" (quit (url->url dir)))))))))

(tm-define (open-remote-file-browser server initial type title cmd)
  (:interactive #t)
  (dialogue-window (remote-file-browser server initial type) cmd title))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-rename-interactive server)
  (:interactive #t)
  (with dir? (remote-directory? (current-buffer))
    (open-remote-file-browser
     server
     (current-buffer)
     (list (if dir? :save-directory :save-file) "Rename as:")
     (if dir? "Rename directory" "Rename file")
     (lambda (new-name)
       (when (url? new-name)
         (remote-rename (current-buffer) new-name))))))

(tm-define (remote-remove-interactive server)
  (:interactive #t)
  (with msg (if (remote-directory? (current-buffer))
                "Really remove directory and its recursive contents?"
                "Really remove file?")
    (user-confirm msg #f
      (lambda (answ)
        (when answ
          (remote-remove (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-permission-all perms server id attr on?)
  (with old-vals (ahash-ref perms attr)
    (with new-vals (append (if on? (list "all") (list))
                           (list-difference old-vals (list "all")))
      (ahash-set! perms attr new-vals)
      (remote-set-field server id attr new-vals))))

(define (set-permissions perms enc server id attr vals*)
  (with vals (map (cut ahash-ref enc <>) vals*)
    (ahash-set! perms attr vals)
    (remote-set-field server id attr vals)))

(define (get-permissions perms dec server id attr)
  (with vals (list-union (ahash-ref perms "owner")
                         (ahash-ref perms attr))
    (with vals* (map (cut ahash-ref dec <>) vals)
      (sort vals* string<=?))))

(tm-widget ((entry-permissions-editor server id attrs users enc dec perms) quit)
  (padded
    (tabs
      (loop (attr attrs)
        (tab (text (upcase-first attr))
          (padded
            (hlist
              (toggle (begin
                        (set-permission-all perms server id attr answer)
                        (refresh-now "permission-checklist"))
                      (in? "all" (get-permissions perms dec server id attr)))
              // //
              (text "All users") >>)
            ===
            (refreshable "permission-checklist"
              (if (nin? "all" (get-permissions perms dec server id attr))
                  (choices (set-permissions perms enc server id attr answer)
                           (sort (map (cut ahash-ref dec <>) users) string<=?)
                           (get-permissions perms dec server id attr)))
              (if (in? "all" (get-permissions perms dec server id attr))
                  (resize "250px" "100px"
                    (text ""))))))))))

(tm-define (open-entry-permissions-editor server id attrs)
  (:interactive #t)
  (with-remote-search-user users server (list)
    (let* ((perms (make-ahash-table))
           (enc (make-ahash-table))
           (dec (make-ahash-table)))
      (with-remote-get-entry entry server id
        (for (attr attrs)
          (with vals (or (assoc-ref entry attr) (list))
            (ahash-set! perms attr vals)
            (set! users (list-union users vals))))
        (with-remote-get-user-pseudo pseudos server users
          (with-remote-get-user-name names server users
            (ahash-set! dec "all" "all")
            (ahash-set! enc "all" "all")
            (for-each (lambda (user pseudo name)
                        (when (and (string? pseudo) (string? name))
                          (with full (string-append pseudo " (" name ")")
                            (ahash-set! dec user full)
                            (ahash-set! enc full user))))
                      users pseudos names)
            (set! users (list-difference users (list "all")))
            (dialogue-window (entry-permissions-editor server id attrs
                                                       users enc dec perms)
                             noop "Change permissions")))))))

(tm-define (open-permissions-editor server u)
  (:interactive #t)
  (with-remote-identifier rid server u
    (when rid
      (with attrs (list "readable" "writable" "owner")
        (open-entry-permissions-editor server rid attrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and database synchronization widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (items-msg l i s)
  (with n (length l)
    (if (== n 1)
        (string-append "1" i " file or directory " s)
        (string-append (number->string n) i " files or directories " s))))

(define (db-msg l i s)
  (with n (length l)
    (if (== n 1)
        (string-append "1" i " database entry " s)
        (string-append (number->string n) i " database entries " s))))

(tm-widget ((simple-list-widget l) quit)
  (padded
    (resize "600px" "400px"
      (scrollable
        (choice (noop) l "")))))

(define (list-item line)
  (with (cmd dir? local-name local-id remote-name remote-id*) line
    ;;(tmfs-cdr (remote-file-name remote-name))))
    (url->system local-name)))

(define (show-list l title)
  (with fl (map list-item l)
    (dialogue-window (simple-list-widget fl) noop title)))

(define (db-list-item line)
  (with (name cmd kind . args) line
    (string-append kind " - " name)))

(define (show-db-list l title)
  (with fl (map db-list-item l)
    (dialogue-window (simple-list-widget fl) noop title)))

(tm-widget ((conflicts-list-widget l t can-skip?) quit)
  (let* ((set-all
          (lambda (action)
            (for (f l)
              (ahash-set! t f action))
            (refresh-now "selected-actions"))))
    (padded
      (hlist (text "Select the action to be undertaken for each conflict") >>)
      ======
      (resize "600px" "400px"
        (scrollable
          (padded
            (refreshable "selected-actions"
              (for (f l)
                (hlist
                  (enum (ahash-set! t f answer)
                        (if can-skip?
                            '("Skip" "Local" "Remote")
                            '("Local" "Remote"))
                        (or (ahash-ref t f)
                            (if can-skip? "Skip" "Remote"))
                        "6em")
                  // //
                  (text f)
                  >>)))
            (glue #f #t 0 0))))
      ======
      (hlist
        (explicit-buttons
          (if can-skip?
              ("Skip all" (set-all "Skip")) // //)
          ("Keep local" (set-all "Local")) // //
          ("Keep remote" (set-all "Remote")) // //
          >>
          ("Ok" (quit)))))))

(define (show-conflicts l t title)
  (with fl (map list-item l)
    (for (f fl) (when (not (ahash-ref t f)) (ahash-set! t f "Skip")))
    (dialogue-window (conflicts-list-widget fl t #t) noop title)))

(define (show-db-conflicts l t title)
  (with fl (map db-list-item l)
    (for (f fl) (when (not (ahash-ref t f)) (ahash-set! t f "Remote")))
    (dialogue-window (conflicts-list-widget fl t #f) noop title)))

(tm-widget ((client-sync-widget server l dbl ltime rtime) quit)
  (let* ((upl (filter-status-list l "upload"))
         (dol (filter-status-list l "download"))
         (ldl (filter-status-list l "local-delete"))
         (rdl (filter-status-list l "remote-delete"))
         (cfl (filter-status-list l "conflict"))
         (t (make-ahash-table))
         (dbupl (db-filter-status-list dbl "upload"))
         (dbdol (db-filter-status-list dbl "download"))
         (dbldl (db-filter-status-list dbl "local-delete"))
         (dbrdl (db-filter-status-list dbl "remote-delete"))
         (dbcfl (db-filter-status-list dbl "conflict"))
         (dbt (make-ahash-table)))
    (padded
      (form "sync-form"
        (explicit-buttons
          ===
          (with msg (items-msg upl "" "to be uploaded")
            (if (nnull? upl)
                (hlist (text msg) // >> ("Details" (show-list upl msg)))))
          ===
          (with msg (items-msg dol "" "to be downloaded")
            (if (nnull? dol)
                (hlist (text msg) // >> ("Details" (show-list dol msg)))))
          ===
          (with msg (items-msg ldl " local" "to be deleted")
            (if (nnull? ldl)
                (hlist (text msg) // >> ("Details" (show-list ldl msg)))))
          ===
          (with msg (items-msg rdl " remote" "to be deleted")
            (if (nnull? rdl)
                (hlist (text msg) // >> ("Details" (show-list rdl msg)))))
          ===
          (with msg (items-msg cfl "" "with conflicts")
            (if (nnull? cfl)
                (hlist (text msg) // >>
                       ("Details" (show-conflicts cfl t msg)))))
          ===
          (with msg (db-msg dbupl "" "to be uploaded")
            (if (nnull? dbupl)
                (hlist (text msg) // >> ("Details" (show-db-list dbupl msg)))))
          ===
          (with msg (db-msg dbdol "" "to be downloaded")
            (if (nnull? dbdol)
                (hlist (text msg) // >> ("Details" (show-db-list dbdol msg)))))
          ===
          (with msg (db-msg dbldl " local" "to be deleted")
            (if (nnull? dbldl)
                (hlist (text msg) // >> ("Details" (show-db-list dbldl msg)))))
          ===
          (with msg (db-msg dbrdl " remote" "to be deleted")
            (if (nnull? dbrdl)
                (hlist (text msg) // >> ("Details" (show-db-list dbrdl msg)))))
          ===
          (with msg (db-msg dbcfl "" "with conflicts")
            (if (nnull? dbcfl)
                (hlist (text msg) // >>
                       ("Details" (show-db-conflicts dbcfl dbt msg))))))
        (if (nnull? upl)
            ===
            (hlist
              (text "Message:") // //
              (form-input "message" "string" (list "") "350px")))
        ===
        (bottom-buttons
          >>
          ("Cancel" (quit)) // //
          ("Synchronize"
           (with msg (or (form-ref "message") "")
             (let* ((r (map (cut requalify-conflicting <> t) l))
                    (dbr (map (cut db-requalify-conflicting <> dbt) dbl)))
               ;;(for (x r)
               ;;(display* "Requalified: " x "\n"))
               ;;(for (x dbr)
               ;;(display* "Requalified: " x "\n"))
               (client-sync-proceed r msg
                 (lambda ()
                   (db-client-sync-proceed server dbr ltime rtime
                     (lambda (ok?)
                       (when (not ok?)
                         (show-message
                          "Extra modifications occurred in the meantime"
                          "Synchronize with remote server"))
                       (quit)))))))))))))

(tm-define (open-sync-widget server l dbl ltime rtime)
  (:interactive #t)
  (if (and (null? l) (null? dbl))
      (begin
        (set-message "up to date" "synchronize with remote server")
        (show-message "Local client is in sync with the remote server"
                      "Synchronize with remote server"))
      (dialogue-window (client-sync-widget server l dbl ltime rtime) noop
                       "Synchronization status")))

(define (client-auto-sync-status l cont)
  (if (null? l)
      (cont (list))
      (client-sync-status (caar l) (cdar l)
        (lambda (r1)
          (client-auto-sync-status (cdr l)
            (lambda (r2)
              (cont (append r1 r2))))))))

(tm-define (client-auto-sync server)
  (client-auto-sync-status (client-auto-sync-list server)
    ;; TODO: filter on server
    (lambda (l)
      ;;(for (x l)
      ;;  (display* "Todo: " x "\n"))
      (db-client-sync-status server
        (lambda (dbl ltime rtime)
          ;;(for (x dbl)
          ;;  (display* "Todo: " x "\n"))
          (open-sync-widget server l dbl ltime rtime))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form fields for files with browse buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (normalize name)
  (if (string-ends? name "/") (normalize (string-drop-right name 1)) name))

(tm-widget (form-local-widget form-name field-name title prompt width)
  (hlist
    (refreshable "local-name"
      (form-input field-name "string" (list (form-ref field-name)) width))
    // //
    (explicit-buttons
      ("Browse"
       (choose-file (lambda (name)
                      (form-set field-name (normalize (url->system name)))
                      (refresh-now "local-name"))
                    title "generic" prompt
                    (with name (form-ref field-name)
                      (if (url-rooted? name)
                          (system->url name)
                          (system->url "~"))))))))

(tm-widget (form-remote-widget server form-name field-name title prompt width)
  (hlist
    (refreshable "remote-name"
      (form-input field-name "string" (list (form-ref field-name)) width))
    // //
    (explicit-buttons
      (when (or (remote-file-name (form-ref field-name))
                (remote-home-directory server))
        ("Browse"
         (open-remote-file-browser
          server
          (with name (form-ref field-name)
            (if (remote-file-name name)
                (system->url name)
                (system->url (remote-home-directory server))))
          (if (== prompt "") :file (list :save-file prompt))
          title
          (lambda (name)
            (form-set field-name (normalize (url->system name)))
            (refresh-now "remote-name"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget for selecting files to be synchronized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (client-auto-sync-data-widget server)
  (padded
    (aligned
      (item (toggle (db-sync-kind server "bib" answer)
                    (db-sync-kind? server "bib"))
        (hlist // // (text "Synchronize bibliographic references") >>)))))

(define (consistent-with? x l)
  (or (null? l)
      (and (not (url-descends? x (car l)))
           (not (url-descends? (car l) x))
           (consistent-with? x (cdr l)))))

(define (consistent? l)
  (or (null? l)
      (and (consistent-with? (car l) (cdr l))
           (consistent? (cdr l)))))

(tm-widget ((client-auto-sync-modify-widget server lname) quit)
  (form "auto-sync"
    (let* ((l (client-auto-sync-list server))
	   (rname (or (assoc-ref l lname) ""))
	   (dummy (begin (form-set "local-name" lname)
			 (form-set "remote-name" rname))))
      (padded
	(aligned
	  (item (text "Local:")
	    (dynamic (form-local-widget "auto-sync" "local-name"
					"Local file or directory"
					"Local:" "350px")))
	  (item (text "Remote:")
	    (dynamic (form-remote-widget server "auto-sync" "remote-name"
					 "Remote file or directory"
					 "Remote:" "350px"))))
	======
	(bottom-buttons
	  >>
	  ("Cancel" (quit #f))
	  // //
	  ("Ok"
	   (let* ((lname* (form-ref "local-name"))
		  (rname* (form-ref "remote-name"))
		  (new-l (assoc-set! (list-copy (assoc-remove! l lname))
				     lname* rname*)))
	     (if (and (consistent? (map system->url (map car new-l)))
		      (consistent? (map system->url (map cdr new-l))))
	       (begin
		 (client-auto-sync-remove server lname)
		 (client-auto-sync-add server lname* rname*)
		 (quit lname*))
	       (show-message
		"Synchronized files and directories should be disjoint"
		"Synchronize file or directory")))))))))

(tm-define (client-auto-sync-modify server lname quit)
  (dialogue-window (client-auto-sync-modify-widget server lname) quit
		   "Synchronize file or directory"))

(tm-widget (client-auto-sync-files-widget server)
  (let* ((selected #f)
	 (select
	  (lambda (which)
            (set! selected (and (!= selected which) which))
            (refresh-now "sync-file-buttons"))))
    (padded
      (refreshable "sync-file-pairs"
        (resize "450px" "250px"
          (scrollable
            (choice (select answer)
                    (map car (client-auto-sync-list server))
                    ""))))
      ======
      (refreshable "sync-file-buttons"
	(hlist
	  (explicit-buttons
            ("Add"
             (client-auto-sync-modify server ""
	       (lambda (new-lname)
                 (refresh-now "sync-file-pairs"))))
            (when selected
              // //
              ("Edit"
               (client-auto-sync-modify server selected
	         (lambda (new-lname)
                   (when new-lname (set! selected new-lname))
                   (refresh-now "sync-file-pairs"))))
              // //
              ("Remove"
               (client-auto-sync-remove server selected)
               (refresh-now "sync-file-pairs")))
            >>))))))

(tm-widget ((client-auto-sync-widget server) quit)
  (form "auto-sync"
    (padded
      (tabs
	(tab (text "Files")
	  (dynamic (client-auto-sync-files-widget server)))
	(tab (text "Data")
	  (dynamic (client-auto-sync-data-widget server))))
      ======
      (hlist
	>>
	(explicit-buttons
	  ("Synchronize" (client-auto-sync server)))))))

(tm-define (remote-interactive-sync server)
  (:interactive #t)
  (dialogue-window (client-auto-sync-widget server) noop
		   "Synchronize with remote server"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upload and download widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((upload-widget server src dest) quit)
  (padded
    (form "upload-form"
      ======
      (aligned
        (item (text "Local source:")
	  (with dummy (form-set "local-name" src)
	    (dynamic (form-local-widget "upload-form" "local-name"
					"Local file or directory"
					"" "300px"))))
        (item (text "Remote destination:")
	  (with dummy (form-set "remote-name" dest)
	    (dynamic (form-remote-widget server "upload-form" "remote-name"
					 "Remote file or directory"
					 "Upload as:" "300px"))))
        (item (text "Upload message:")
          (form-input "message" "string" (list "") "300px")))
      ======
      (bottom-buttons
        >>
        ("Cancel" (quit)) // //
        ("Upload"
	 (let* ((local-name (system->url (form-ref "local-name")))
		(remote-name (system->url (form-ref "remote-name")))
		(message (form-ref "message")))
	   (when (and (url-exists? local-name)
		      (remote-file-name remote-name))
	     (when (and (url-regular? local-name)
			(remote-directory? remote-name))
	       (set! remote-name (url-append remote-name
					     (url-tail local-name))))
	     (if (and (url-directory? local-name)
		      (remote-file? remote-name))
		 (set-message "cannot upload directory to file" "upload")
		 (begin
		   ;;(display* "Local  : " local-name "\n")
		   ;;(display* "Remote : " remote-name "\n")
		   ;;(display* "Message: " message "\n")
		   (remote-upload local-name remote-name message
                                  (lambda x (revert-buffer-revert)))))
	     (quit))))))))

(tm-define (remote-interactive-upload server)
  (:interactive #t)
  (let* ((src  (if (remote-file-name (current-buffer)) ""
                   (url->system (current-buffer))))
         (dest (if (remote-file-name (current-buffer))
                   (url->system (current-buffer)) "")))
    (dialogue-window (upload-widget server src dest) noop
                     "Upload file or directory")))

(tm-widget ((download-widget server) quit)
  (padded
    (form "download-form"
      ======
      (aligned
        (item (text "Remote source:")
	  (with dummy (form-set "remote-name"
                                (if (remote-file-name (current-buffer))
                                    (url->system (current-buffer)) ""))
	    (dynamic (form-remote-widget server "download-form" "remote-name"
					 "Remote file or directory"
					 "" "300px"))))
        (item (text "Local destination:")
	  (with dummy (form-set "local-name"
                                (if (remote-file-name (current-buffer)) ""
                                    (url->system (current-buffer))))
	    (dynamic (form-local-widget "download-form" "local-name"
					"Local file or directory"
					"Download as:" "300px")))))
      ======
      (bottom-buttons
        >>
        ("Cancel" (quit)) // //
        ("Download"
	 (let* ((remote-name (system->url (form-ref "remote-name")))
		(local-name (system->url (form-ref "local-name"))))
	   (when (and (remote-file-name remote-name)
		      (or (url-exists? local-name)
			  (url-exists? (url-head local-name))))
	     (when (and (remote-file? remote-name)
			(url-directory? local-name))
	       (set! local-name (url-append local-name
					    (url-tail remote-name))))
	     (if (and (remote-directory? remote-name)
		      (url-regular? local-name))
		 (set-message "cannot download directory to file" "download")
		 (begin
		   ;;(display* "Remote : " remote-name "\n")
		   ;;(display* "Local  : " local-name "\n")
		   (remote-download local-name remote-name)))
	     (quit))))))))

(tm-define (remote-interactive-download server)
  (:interactive #t)
  (dialogue-window (download-widget server) noop
		   "Download file or directory"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct upload and download dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simple-upload server local-name remote-dir)
  (when (and (url-exists? local-name)
             (remote-file-name remote-dir)
             (url-regular? local-name)
             (remote-directory? remote-dir))
    (with remote-name (url-append remote-dir (url-tail local-name))
      (remote-upload local-name remote-name "uploaded"
                     (lambda x
                       (revert-buffer-revert)
                       (set-message "upload completed" "upload"))))))

(tm-define (simple-interactive-upload server)
  (:interactive #t)
  (choose-file (lambda (local-name)
                 (simple-upload server local-name (current-buffer)))
               "Upload file or directory" "generic" ""
               (system->url "$PWD")))

(define (simple-download server remote-name local-name)
  (when (and (remote-file-name remote-name)
             (or (url-exists? local-name)
                 (url-exists? (url-head local-name))))
    (when (and (remote-file? remote-name)
               (url-directory? local-name))
      (set! local-name (url-append local-name (url-tail remote-name))))
    (remote-download local-name remote-name
                     (lambda x
                       (set-message "download completed" "download")))))

(tm-define (simple-interactive-download server)
  (:interactive #t)
  (let* ((name (current-buffer))
         (type (if (remote-directory? name) "directory" "generic"))
         (tail (if (remote-home-directory? name) "Home" (url-tail name)))
         (prop (url-append (system->url "$PWD") tail)))
    (choose-file (lambda (local-name)
                   (simple-download server name local-name))
                 "Download file or directory" type "Download" prop)))

(define (simple-synchronize server remote-name local-name)
  (when (and (remote-file-name remote-name)
             (or (url-exists? local-name)
                 (url-exists? (url-head local-name))))
    (when (and (remote-file? remote-name)
               (url-directory? local-name))
      (set! local-name (url-append local-name (url-tail remote-name))))
    (client-auto-sync-add server (url->system local-name)
                                 (url->system remote-name))
    (remote-interactive-sync server)))

(tm-define (simple-interactive-synchronize server)
  (:interactive #t)
  (let* ((name (current-buffer))
         (type (if (remote-directory? name) "directory" "generic"))
         (tail (if (remote-home-directory? name) "Home" (url-tail name)))
         (prop (url-append (system->url "$PWD") tail))
         (title (string-append
                  "Synchronize with current "
                  (if (== type "directory") "directory" "file"))))
    (choose-file (lambda (local-name)
                   (simple-synchronize server name local-name))
                 title type "Synchronize" prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message-send server u to)
  (and-let* ((doc (buffer-get-body u))
             (msg (tm->stree doc))
             (dest (map (cut string-append "mail-" <>) to))
             (cmd `(remote-send-message ,dest "send-document" ,msg)))
    (client-remote-eval server cmd
      (lambda x
        (set-message "message sent" "instant message")
        (show-message "Your message has been sent."
                      "Send instant message")))))

(tm-widget ((message-editor server u users to) quit)
  (padded
    (horizontal
      (vertical
        (bold (text "To"))
        === ===
        (resize "250px" "350px"
          (choices (set! to answer) (sort users string<=?) to)))
      ///
      (vertical
        (bold (text "Message"))
        === ===
        (resize "500px" "350px"
          (texmacs-input `(document "") `(style (tuple "generic")) u))))
    ======
    (hlist
      >>
      (explicit-buttons
	("Send" (message-send server u to) (quit))))))

(tm-define (open-message-editor server)
  (:interactive #t)
  (with-remote-search-user users server (list)
    (let* ((b (current-buffer-url))
           (u (string->url "tmfs://aux/message-editor")))
      (dialogue-window (message-editor server u users (list))
                       (lambda x (noop))
                       "Message editor" u)
      (buffer-set-master u b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sharing documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (share-document server u to)
  (and-let* ((msg (url->string u))
             (dest (map (cut string-append "mail-" <>) to))
             (cmd `(remote-send-message ,dest "share" ,msg)))
    (client-remote-eval server cmd
      (lambda x
        (set-message (if (chat-room-url? u)
                         "invitation sent"
                         "document shared")
                     "instant message")
        (show-message (if (chat-room-url? u)
                          "Your invitation has been sent."
                          "Your document has been shared.")
                      "Send instant message")))))

(tm-widget ((share-document-widget server u users to) quit)
  (padded
    (with s (if (chat-room-url? u) "Invite" "Share with")
      (bold (text s)))
    === ===
    (resize "250px" "350px"
      (choices (set! to answer) (sort users string<=?) to))
    ======
    (hlist
      >>
      (explicit-buttons
	("Send" (share-document server u to) (quit))))))

(tm-define (open-share-document-widget server u)
  (:interactive #t)
  (with-remote-search-user users server (list)
    (with-remote-identifier rid server u
      (when rid
        (with-remote-get-entry entry server rid
          (with to (list-union (or (assoc-ref entry "readable") (list))
                               (or (assoc-ref entry "owner") (list)))
            (if (in? "all" to) (set! to users) (set! users to))
            (dialogue-window (share-document-widget server u users to)
                             (lambda x (noop))
                             "Instant message")))))))
