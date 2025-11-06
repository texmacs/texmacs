
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-widgets.scm
;; DESCRIPTION : widgets for server
;; COPYRIGHT   : (C) 2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-widgets)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((server-info-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (server-open-success s)
  (:interactive #t)
  (dialogue-window (server-info-widget s) noop "Success"))

(tm-define (server-open-error s)
  (:interactive #t)
  (dialogue-window (server-info-widget s) noop "Error"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (server-preferences-widget quit)
  (refreshable "server-preferences-refresh"
  (padded
    (bold (text "Connection"))
    ===
    (aligned
      (item (text "Port number:")
	(hlist // (input (when answer (server-set-port answer))
	  "string" (list (server-get-port)) "5em")))
      (item (text "Contact timeout in milliseconds:")
	(hlist // (input (when answer (server-set-contact-timeout answer))
	  "string" (list (number->string (server-get-contact-timeout))) "5em")))
      (item (text "Connection timeout in seconds:")
	(hlist // (input (when answer (server-set-connection-timeout answer))
	  "string" (list (number->string (server-get-connection-timeout))) "5em"))))
    === ===
    (bold (text "Authentications"))
    ===
    (aligned
      (meti (hlist // (text "Anonymous via TLS"))
	(toggle (begin (set-boolean-preference
			"tls-server authentication anonymous" answer)
		       (refresh-now "server-preferences-refresh"))
		(get-boolean-preference
		 "tls-server authentication anonymous"))))
    === ===
    (bold (text "Account security"))
    ===
    (aligned
      (item (text "Failed login limit:")
	(hlist // (input (when answer (server-set-failed-login-limit answer))
	  "string" (list (number->string (server-get-failed-login-limit))) "5em")))
      (item (text "Failed login delay in seconds:")
	(hlist // (input (when answer (server-set-failed-login-delay answer))
	  "string" (list (number->string (server-get-failed-login-delay)))
	  "5em")))
      (item (text "Account confirmation delay in seconds:")
	(hlist // (input (when answer
			   (server-set-account-confirmation-delay answer))
	  "string" (list (number->string (server-get-account-confirmation-delay)))
	  "5em")))
      (item (text "Credentials resetting delay in seconds:")
	(hlist // (input (when answer
			   (server-set-reset-credentials-delay answer))
	  "string" (list (number->string (server-get-reset-credentials-delay)))
	  "5em"))))
    === ===
    (bold (text "Services"))
    ===
    (when (get-boolean-preference "tls-server authentication anonymous")
    (aligned
      (meti (hlist // (text "Remote password login"))
	(toggle (set-boolean-preference
		 "server service login" answer)
		(get-boolean-preference
		 "server service login")))
      (meti (hlist // (text "Remote account creation"))
	(toggle (set-boolean-preference
		 "server service new-account" answer)
		(get-boolean-preference
		 "server service new-account")))
      (meti (hlist // (text "Remote credentials resetting"))
	(toggle (set-boolean-preference
		 "server service reset-credentials" answer)
		(get-boolean-preference
		 "server service reset-credentials")))
;      (meti (hlist // (text "Public preferences"))
;	(toggle (set-boolean-preference
;		 "server service public preferences" answer)
;		(get-boolean-preference
;		 "server service public preferences")))
      ))
    === ===
    (bold (text "Passwords"))
    ===
    (aligned
      (meti (hlist // (text "Password encoding"))
	(enum (set-preference "server password encoding" answer)
	      (server-supported-password-encodings)
	      (get-preference "server password encoding") "4em"))
      (meti (hlist // (text "Require strong passwords"))
	(toggle (set-boolean-preference
		 "server require strong passwords" answer)
		(get-boolean-preference
		 "server require strong passwords")))
      (meti (hlist // (text "Automatic encoding update"))
	(toggle (set-boolean-preference
		 "server password update" answer)
		(get-boolean-preference
		 "server password update")))))))

(tm-define (open-server-preferences)
  (dialogue-window server-preferences-widget noop
		   "Server preferences"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server Certificate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cert-url (string->url "$TEXMACS_HOME_PATH/server/cert.pem"))
(define key-url  (string->url "$TEXMACS_HOME_PATH/server/key.pem"))

(tm-define (server-certificate-exists?) (url-exists? cert-url))

(tm-widget (certificate-created-widget quit)
  (padded
    (vlist
      (centered (text "Certificated sucessfully created."))
      ===
      (centered (text "Would you like to start the server?"))
      >>)
    ======
    (hlist
      (explicit-buttons
        ("Cancel" (quit))
        >>
        ("Start" (begin (server-start) (quit)))))))

(tm-widget (certificate-manager-widget quit)
  (let* ((action (if (server-certificate-exists?) "Renew" "Create")))
    (padded
      (form "certificate-manager-info"
        (centered (bold (text (string-append action " Certificate"))))
        ===
        (centered (text (string-append
                "Please provide the certificate information. The fields below"
                " are optional.<#OA>This will generate a self-signed certificate.")))
        (centered (text (string-append
                "If this server is intended for public use, we recommend"
                " obtaining<#0A>a certificate signed by a trusted Certificate"
                " Authority.")))
        ======
        (aligned
          (item (text "Common Name:")
                (form-input "cn" "string" '() "280px"))
          (item (text "Country:")
                (form-input "country" "string" '() "280px"))
          (item (text "City/Locality:")
                (form-input "locality" "string" '() "280px"))
          (item (text "State:")
                (form-input "state" "string" '() "280px"))
          (item (text "Organization:")
                (form-input "organization" "string" '() "280px"))
          (item (text "Unit:")
                (form-input "unit" "string" '() "280px")))
        ======
        (bottom-buttons
          >>
          ("Cancel" (quit))
          // //
          ("Create"
           (with cfg (make-ahash-table)
             (for-each (cut ahash-set! cfg <> <>) (form-fields) (form-values))
             (if (generate-self-signed-certificate
                       (ahash-fold
                         (lambda (k v l) (if v (cons (list k v) l) l)) '() cfg)
                       cert-url
                       key-url)
               (begin (quit) (open-certificate-created))
               (server-open-error
                 (string-append "Could not create server certificate, "
                                "check texmacs console"))))))))))

(tm-widget ((certificate-warning-widget message can-skip?) quit)
  (padded
    (vlist
      (centered (bold (text message)))
      (centered (bold (text (string-append "Path: " (url->string cert-url)))))
      ===
      (text "Would you like to open the certificate manager?")
      ===
      (text
        "This will allow you to create or renew the server certificate.")
      (text
        "Otherwise you would need to create one manually or obtain one from an authoritative source")
      >>)
    ======
    (hlist
      (explicit-buttons
        ("Cancel" (quit))
        >>
        (if can-skip?
          ("Skip" (server-start)) // //)
        >>
        ("Open Manager" (begin (quit) (open-certificate-manager)))))))

(tm-define (open-certificate-manager)
  (dialogue-window certificate-manager-widget noop "Certificate manager"))

(tm-define (open-certificate-created)
  (dialogue-window certificate-created-widget noop "Success"))

(tm-define (open-certificate-warning message can-skip?)
  (dialogue-window
    (certificate-warning-widget message can-skip?) noop "Certificate warning"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key? is for future use of public keys
(define (first-incomplete-field t pw? key?)
  (cond ((== (ahash-ref* t "pseudo" "") "") "Pseudo")
	((== (ahash-ref* t "name" "") "") "Full name")
	((== (ahash-ref* t "email" "") "") "Email")
	((and pw? (== (ahash-ref* t "password" "") "")) "Password")
	(else #f)))

(tm-widget (server-add-account-widget quit)
  (let* ((dummy (form-named-set "server-account-info" "password" ""))
         (auth "Password")
	 (pseudo (list))
	 (name (list))
	 (email (list))
         (use-password? #f)
         (use-key? #f))
    (padded
      (refreshable "server-account-refresh"
      (form "server-account-info"
        (hlist >> (aligned
          (item (text "Pseudo:")
            (form-input "pseudo" "string" pseudo "300px"))
          (item (text "Full name:")
            (form-input "name" "string" name "300px"))
          (item (text "Email:")
            (form-input "email" "string" email "300px"))
	  (item (text "Administrator:")
	    (form-toggle "admin" #f))))
        ===
        (hlist >> (aligned (item (text "Authentication:")
          (enum (begin (set! auth answer)
                       (refresh-now "server-auth-info"))
                 (list "Password") auth "280px"))) // //)
        ===
        (refreshable "server-auth-info"
          (if (== auth "Password")
            (hlist >> (aligned
              (item (text "Password:")
                (form-input "password" "password"
                            (list (fifth (form-values))) "300px"))) // //)))
        ======
        (bottom-buttons
          >>
          ("Cancel"
	   (quit)) // // //
          ("Search"
	   (when (nnull? (form-values))
	     (set! pseudo (first (form-values)))
	     (and-with uid (server-find-user pseudo)
	       (with-database (server-database)
		 (set! pseudo (db-get-field uid "pseudo"))
		 (set! name (db-get-field uid "name"))
		 (set! email (db-get-field uid "email"))
		 (refresh-now "server-account-refresh"))))) // // //
   	  ("Proceed"
           (with t (make-ahash-table)
             (for-each (cut ahash-set! t <> <>) (form-fields) (form-values))
             (when (!= (ahash-ref* t "password" "") "")
               (set! use-password? #t))
             (cond ((server-find-user (ahash-ref t "pseudo"))
		    (server-open-error (string-append
				 "User '" (ahash-ref t "pseudo")
				 "' already exists")))
		   ((not (or use-password? use-key?))
                    (server-open-error
                     "Please fill at least one authentification method"))
                   ((first-incomplete-field t use-password? use-key?)
		    (server-open-error (string-append "Please fill all fields")))
                   (else
                     (with creds `()
                       (when use-password?
                         (set! creds
                               (cons `(password
                                       ,(ahash-ref t "password"))
                                     creds)))
                       (server-set-user-information
			(ahash-ref t "pseudo")
			(ahash-ref t "name")
			(server-hide-credentials (server-add-salt creds (generate-salt 128)))
			(ahash-ref t "email")
			(if (ahash-ref t "admin") "yes" "no"))
                       (quit))))))))))))

(tm-define (open-server-account-creator)
  (:interactive #t)
  (dialogue-window
   server-add-account-widget noop "Create server account"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((server-edit-account-widget uid2) quit)
  (with uid (if (!= uid2 "") uid2 server-accounts-widget-uid)
    (with (pseudo name credentials email admin)
	(server-get-user-info uid)
      (let* ((suspended (with-database (server-database)
			  (db-get-field uid "suspended")))
	     (edit-password? #f)
	     (hiddens (server-credentials-normalize credentials))
	     (password (with l (filter
				(lambda (x)
				  (and (list? x) (nnull? x)
				       (== (first x) `password))) hiddens)
			 (if (null? l) #f (car l)))))
	(padded
	  (refreshable "server-edit-account-refreshable"
	    (form "server-edit-account-form"
	      (aligned
		(item (text "Pseudo:") (text pseudo))
		(item (text "Full name:")
		  (form-input "name" "string" (list name) "300px"))
		(item (text "Email:")
		  (form-input "email" "string" (list email) "300px"))
		(item (text "Administrator:")
		  (form-toggle "admin" admin))
		(item (text "Suspended:")
		  (form-toggle "suspended" (== suspended `("#t"))))
		(item (text "Password:")
		  (hlist
		    (if edit-password?
			(form-input "password" "password" (list "") "300px")) 
		    (explicit-buttons
		      (if (and (not edit-password?) (not password))
			  ("Add"
			   (set! edit-password? #t)
			   (refresh-now "server-edit-account-refreshable"))
			  // // //)
		      (if (and (not edit-password?) password)
			  ("Delete"
			   (set! password #f)
			   (refresh-now "server-edit-account-refreshable"))
			  // // //
			  ("Change"
			   (set! edit-password? #t)
			   (refresh-now "server-edit-account-refreshable"))))
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
		     (set! name (or (ahash-ref t "name") ""))
		     (set! email (or (ahash-ref t "email") ""))
		     (set! admin (ahash-ref t "admin"))
		     (set! hiddens `())
		     (when (and edit-password? (ahash-ref t "password"))
		       (set! password
			     (server-hide-credential
			      `(password
				,(ahash-ref t "password")))))
		     ;; todo << better support for empty password
		     (when password
		       (set! hiddens (cons password hiddens)))
		     (server-set-user-info
		      uid pseudo name hiddens email admin)
		     (if (ahash-ref t "suspended")
			 (server-suspend-user pseudo)
			 (server-resume-user pseudo))
		     (quit))))))))))))
    
(tm-define (open-server-account-editor . uid)
  (:interactive #t)
  (dialogue-window
   (server-edit-account-widget (if (null? uid) "" (car uid)))
   noop "Edit server account"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-all-accounts-user-list)
  (with-database (server-database)
    (db-search `(("type" "user")))))

(define (get-accounts-pseudo-list)
  (map server-uid->pseudo (get-all-accounts-user-list)))

(define (get-accounts-info-list)
  (map server-user->info (get-all-accounts-user-list)))

(define server-user->info-table (make-ahash-table))

(define (server-user->info uid)
  (with-database (server-database)
    (if (or (nstring? uid) (null? (db-get-entry uid))) ""
	(let* ((pseudo (db-get-field uid "pseudo"))
	       (name (db-get-field uid "name"))
	       (email (db-get-field uid "email"))
	       (suspended (db-get-field uid "suspended")))
	  (set! pseudo (if (nnull? pseudo) (car pseudo) "???"))
	  (set! name (if (nnull? name) (car name) "???"))
	  (set! email (if (nnull? email) (car email) "???"))
	  (with info (string-append pseudo ", " name ", " email)
	    (ahash-set! server-user->info-table info uid)
	    info)))))

(define server-accounts-widget-uid "")

(tm-widget (server-accounts-widget quit)
  (with uid ""
    (padded
      (resize '("450px" "450px" "9999px") '("250px" "250px" "9999px")
	(refreshable "server-accounts-choice-refreshable"
	  (scrollable
	    (choice (begin
		      (set! uid (ahash-ref server-user->info-table answer))
		      (refresh-now "server-accounts-buttons-refreshable"))   
		    (sort (get-accounts-info-list) string<=?)
		    (server-user->info uid)))))
      ===
      (refreshable "server-accounts-buttons-refreshable"
	(hlist >> (explicit-buttons
		    ("Cancel" (quit))  // // //       
		    (when (!= uid "")
		      ("Edit"
		       (set! server-accounts-widget-uid uid)
		       (open-server-account-editor)) // // //
		       ("Delete"
		       (server-open-error "not implemented")) // // //)
		    ("Add"
		     (dialogue-window server-add-account-widget 
		      (lambda ()
			(refresh-now "server-accounts-choice-refreshable"))
		      "Create server account"))))))))

(tm-define (open-server-accounts-manager)
  (:interactive #t)
  (dialogue-window
   server-accounts-widget
   noop "Server accounts"))
