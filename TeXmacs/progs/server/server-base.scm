
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-base.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;                   2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-base)
  (:use (server server-authentication)
        (security password)))

(server-define-error-codes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-database) (global-database))

(tm-define service-dispatch-table (make-ahash-table))

(tm-define-macro (tm-service proto . body)
  (if (npair? proto) '(noop)
      (with (fun . args) proto
	 `(begin
           (tm-define (,(symbol-append 'service- fun) envelope ,@args)
             (with-database (server-database)
               (catch #t
                      (lambda () ,@body)
                      (lambda (key . err-args)
                        (with msg (apply format-err key err-args)
                              (server-log-write `error
                                (string-append "Server error in service "
                                               (symbol->string ',fun) ": " msg))
                              (server-error envelope msg))))))
           (ahash-set! service-dispatch-table
                       ',fun ,(symbol-append 'service- fun))))))

(tm-define (server-eval envelope cmd)
  (when (debug-get "remote")
    (display* "server-eval " envelope ", " cmd "\n"))
  (cond ((and (pair? cmd) (ahash-ref service-dispatch-table (car cmd)))
         (with (name . args) cmd
           (with fun (ahash-ref service-dispatch-table name)
             (apply fun (cons envelope args))))) ;; todo << catch errors
        ((symbol? (car cmd))
         (with s (symbol->string (car cmd))
           (server-error envelope (string-append "invalid command '" s "'"))))
        (else (server-error envelope "invalid command"))))

(tm-define (server-return envelope ret-val)
  (with (client msg-id) envelope
    (server-send client `(client-remote-result ,msg-id ,ret-val))))

(tm-define (server-error envelope error-msg)
  (with (client msg-id) envelope
    (server-send client `(client-remote-error ,msg-id ,error-msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-client-active? (make-ahash-table))
(define server-serial 0)

(tm-define (active-client? client)
  (ahash-ref server-client-active? client))

(tm-define (active-clients)
  (ahash-set->list server-client-active?))

(tm-define (server-send client cmd)
  (server-write client (object->string* (list server-serial cmd)))
  (set! server-serial (+ server-serial 1)))

(tm-define (server-add client)
  (ahash-set! server-client-active? client #t)
  (with wait 1
    (delayed
      (:while (ahash-ref server-client-active? client))
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      ;;(display* "server-wait= " wait "\n")
      (with msg (server-read client)
        ;;(when (!= msg "")
        ;;  (display* "wait  = " wait "\n")
        ;;  (display* "client= " client "\n")
        ;;  (display* "msg   = " msg "\n"))
        (when (!= msg "")
          (with (msg-id msg-cmd) (string->object msg)
            (server-eval (list client msg-id) msg-cmd)
            (set! wait 1)))))))

(tm-define (server-remove client)
  (ahash-remove! server-client-active? client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-continuations (make-ahash-table))
(define server-error-handlers (make-ahash-table))
 
(define (std-server-error msg)
  ;;(texmacs-error "server-remote-error" "remote error ~S" msg)
  (server-log-write 'error msg)
  (display-err* "Remote error: " msg "\n"))

(tm-define (server-remote-eval client cmd cont . opt-err-handler)
  (with err-handler std-server-error
    (if (nnull? opt-err-handler) (set! err-handler (car opt-err-handler)))
    (ahash-set! server-continuations server-serial (list client cont))
    (ahash-set! server-error-handlers server-serial (list client err-handler))
    (server-send client cmd)))

(tm-define (server-remote-eval* client cmd cont)
  (server-remote-eval client cmd cont cont))

(tm-service (server-remote-result msg-id ret)
  (with client (car envelope)
    (when (debug-get "remote")
      (display* "server-remote-result " (list client msg-id) "\n"))
    (and-with val (ahash-ref server-continuations msg-id)
      (ahash-remove! server-continuations msg-id)
      (ahash-remove! server-error-handlers msg-id)
      (with (orig-client cont) val
        (when (== client orig-client)
          (cont ret))))))

(tm-service (server-remote-error msg-id err-msg)
  (with client (car envelope)
    (and-with val (ahash-ref server-error-handlers msg-id)
      (ahash-remove! server-continuations msg-id)
      (ahash-remove! server-error-handlers msg-id)
      (with (orig-client err-handler) val
        (when (== client orig-client)
	  (err-handler err-msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pending users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! *random-state* (seed->random-state (current-time)))

(define server-pending-users (make-ahash-table))

(define (server-load-pending-users)
  (when (== (ahash-size server-pending-users) 0)
    (with f "$TEXMACS_HOME_PATH/server/pending-users.scm"
      (set! server-pending-users
            (if (url-exists? f)
                (list->ahash-table (load-object f))
                (make-ahash-table))))))

(define (server-save-pending-users)
  (with f "$TEXMACS_HOME_PATH/server/pending-users.scm"
    (save-object f (ahash-table->list server-pending-users))))

(tm-define (server-clean-pending-users)
  (with d (server-get-account-confirmation-delay)
    (when (>= d 0)
      (server-load-pending-users)
      (let* ((l (map car (ahash-table->list server-pending-users)))
	     (t (- (current-time) d)))
	(for (pseudo l)
	  (with u (ahash-ref server-pending-users pseudo)
	    (when (< (string->number (fifth u)) t)
	      (server-log-write `log-notice
	       (string-append "removing pending user " pseudo))
	      (ahash-remove! server-pending-users pseudo)))))
      (server-save-pending-users))))

(tm-define (server-find-pending-user pseudo)
  (server-load-pending-users)
  (ahash-ref server-pending-users pseudo))

(tm-define (server-get-pending-code pseudo)
  (and-with l (server-find-pending-user pseudo)
    (sixth l)))

(tm-define (server-pending-pseudo-exists? pseudo)
  (server-clean-pending-users)
  (if (server-find-pending-user pseudo) #t #f))

(tm-define (server-create-pending-user pseudo name hiddens email admin)
  (server-load-pending-users)
  (when (not (server-find-pending-user pseudo))
    (ahash-set! server-pending-users pseudo
		(list name hiddens email admin
		      (number->string (current-time))
		      (number->string (random 1000000))))
    (server-save-pending-users)))

(tm-define (server-remove-pending-user pseudo)
  (server-load-pending-users)
  (when (server-find-pending-user pseudo)
    (ahash-remove! server-pending-users pseudo)
    (server-save-pending-users)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-users (make-ahash-table))

(define (server-load-users)
  (when (== (ahash-size server-users) 0)
    (with f "$TEXMACS_HOME_PATH/server/users.scm"
      (set! server-users
            (if (url-exists? f)
                (list->ahash-table (load-object f))
                (make-ahash-table))))))

(define (server-save-users)
  (with f "$TEXMACS_HOME_PATH/server/users.scm"
    (save-object f (ahash-table->list server-users))))

(tm-define (server-find-user pseudo)
  (server-load-users)
  (with l (ahash-table->list server-users)
    (with ok? (lambda (x) (== (cadr x) pseudo))
      (and-with i (list-find-index l ok?)
        (car (list-ref l i))))))

(tm-define (server-uid->pseudo uid)
  (with-database (server-database)
    (with l (db-get-field uid "pseudo")
      (and (nnull? l) (car l)))))

(tm-define (server-get-user-info uid)
  (server-load-users)
  (ahash-ref server-users uid))

(define (get-accounts-user-list limit offset)
  (with-database (server-database)
    (server-load-users)
    (let* ((users (db-search-paginate `(("type" "user")) limit offset)))
          users)))

(define (user-info->list uid)
  (with (pseudo name credentials email admin)
        (ahash-ref server-users uid)
        `(("pseudo" ,pseudo)
          ("name" ,name)
          ("authentications"
           ,(credentials->authentications credentials))
          ("email" ,email)
          ("admin" ,admin))))

(tm-define (server-set-user-info uid pseudo name hiddens email admin)
  (with-database (server-database)
    (with-user #t
      (when (not uid) (set! uid pseudo))
      ;;(when (not uid) (set! uid (db-create-entry (list))))
      (db-set-entry uid (list (list "type" "user")
			      (list "pseudo" pseudo)
			      (list "name" name)
			      `(credentials . ,hiddens)
			      (list "email" email)
			      (list "owner" uid)))
      (server-load-users)
      (ahash-set! server-users uid (list pseudo name hiddens email admin))
      (server-save-users)
      (let* ((home (string-append "~" pseudo))
	     (q (list (list "name" home)
		      (list "type" "dir"))))
	(when (null? (db-search q))
	  (db-create-entry (rcons q (list "owner" uid))))))))

(tm-define (server-set-user-information pseudo name hiddens email admin)
  (:argument pseudo "User pseudo")
  (:argument name "Full name")
  (:argument hiddens "Hidden credentials")
  (:argument email "Email address")
  (:argument admin "Administrive rights?")
  (:proposals admin '("no" "yes"))
  ;; (server-log-format 'info "setting user information for ~S: ~A ~A ~A ~A"
  ;;                    pseudo name hiddens email admin)
  (with uid (server-find-user pseudo)
    (server-set-user-info uid pseudo name hiddens email (== admin "yes"))))

(tm-define (server-remove-user pseudo) ;; todo << fixme
  (with-database (server-database)
    (let* ((uid (server-find-user pseudo))
           (home (string-append "~" pseudo))
           (q (list (list "name" home)
                    (list "type" "dir"))))
      (server-dir-remove uid (string-append "~" pseudo) #f)
      (and-with p (db-search q)
                (db-remove-entry p)))
    (db-remove-entry uid)
    (server-load-users)
    (ahash-remove! server-users uid)
    (server-save-users)))

(tm-define (server-remove-account pseudo)
  (if (not (or (server-pseudo-exists? pseudo)
               (server-pending-pseudo-exists? pseudo)))
    (not (server-log-format
           `warning "could not delete user ~a, user does not exist\n" pseudo))
    (or (and (server-pseudo-exists? pseudo) (server-remove-user pseudo))
        (and (server-pending-pseudo-exists? pseudo)
             (server-remove-pending-user pseudo)))))

(tm-define (server-suspend-user pseudo)
  (with-database (server-database)
    (with ids (db-search `(("type" "user")
                           ("pseudo" ,pseudo)))
      (and (nnull? ids) (db-set-field (car ids) "suspended" (list "#t"))))))

(tm-define (server-resume-user pseudo)
  (with-database (server-database)
    (with ids (db-search `(("type" "user")
                           ("pseudo" ,pseudo)))
      (and (nnull? ids) (db-remove-field (car ids) "suspended")))))

(tm-define (server-user-suspended? pseudo)
  (with-database (server-database)
    (with ids (db-search `(("type" "user")
                           ("pseudo" ,pseudo)))
      (and (nnull? ids)
           (== (db-get-field (car ids) "suspended") (list "#t"))))))

(tm-define (server-pseudo-exists? pseudo)
  (server-find-user pseudo))

(tm-define (server-reset-preferences)
  (for-each (lambda (pref) (reset-preference (car pref)))
            (server-admin-preferences)))

(tm-define (server-reset-admin-password)
  (let* ((info (server-get-user-info "admin"))
         (passwd (generate-password 20))
         (credentials (server-add-salt (cons `(tls-password ,passwd) '())
                                       (generate-salt)))
         (hiddens (server-hide-credentials credentials)))
    (when info
      (server-set-user-info #f "admin" "admin" hiddens (fourth info) #t)
      (server-log-write `warning
        (string-append "The password for the admin account has been reset.\n"
		       "login: admin \n"
		       "password: " passwd "\n"))
      (when (not (headless?))
	(open-auxiliary "New password for admin at localhost"
          `(document (style "generic")
	     (body (document "New password for admin at localhost"
			     "login: admin"
			     ,(string-append "password: " passwd)))))))))

(tm-define (server-create-default-admin-account)
  (server-load-users)
  (when (== (ahash-size server-users) 0)
    (let* ((passwd (generate-password 20))
           (credentials (server-add-salt (cons `(tls-password ,passwd) '())
                                         (generate-salt)))
           (hiddens (server-hide-credentials credentials)))
      (server-set-user-info #f "admin" "admin" hiddens "admin" #t)
      (server-log-write `warning
	(string-append
	 "An administrator account has been created.\n"
	 "login: admin \n"
	 "password: " passwd "\n"
	 "Please change this password as your first action to ensure the "
	 "security of your server.\n"))
      (when (not (headless?))
	(open-auxiliary "New password for admin at localhost"
          `(document (style "generic")
	     (body (document "New password for admin at localhost"
			     "login: admin"
			     ,(string-append "password: " passwd)))))))))

(tm-service (new-account pseudo name credentials email agreed)
  (if (!= (get-preference "server service new-account") "on")
      (server-return envelope "remote account creation is not allowed")
      (if (and (!= (get-preference "server require strong passwords") "off")
	       (not (server-strong-credentials? credentials)))
	  (server-return envelope server-strong-password-instructions)
	  (if (or (server-pseudo-exists? pseudo)
		  (server-pending-pseudo-exists? pseudo))
	      (server-return envelope "user already exists")
	      (let* ((salt (generate-salt))
		            (credentials (server-add-salt credentials salt))
		            (hiddens (server-hide-credentials credentials)))
                ;(display* "hiddens: " hiddens "\n")
		(if (and (>= (server-get-account-confirmation-delay) 0)
                         (not (server-check-admin? envelope)))
		    (begin
		      (server-create-pending-user pseudo name hiddens email #f)
		      (server-send-email-new-account
		       pseudo name email (server-get-pending-code pseudo))
		      (server-return envelope "pending"))
		    (begin
		      (server-set-user-info #f pseudo name hiddens email #f)
		      (server-return envelope "done"))))))))

(tm-service (remote-get-accounts limit offset)
  (if (server-check-admin? envelope)
    (server-return envelope (get-accounts-user-list limit offset))
    (server-return envelope "remote accounts list is not allowed")))

(tm-service (remote-remove-accounts pseudos)
  (if (and (== (get-preference "server service new-account") "on")
           (server-check-admin? envelope))
    (server-return envelope
      (list-fold
        (lambda (pseudo removed)
          (if (server-remove-account pseudo) (cons pseudo prev) prev))
        '() pseudos))
    (server-return envelope "remote account removal is not allowed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User security
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-get-user-last-login-time-uid uid)
  (with-database (server-database)
    (with t (db-get-field uid "last-login-time")
      (if (null? t) 0 (string->number (car t))))))

(tm-define (server-set-user-last-login-time-uid uid t)
  (with-database (server-database)
    (db-set-field uid "last-login-time" (list (number->string t)))))

(tm-define (server-get-user-last-login-address-uid uid)
  (with-database (server-database)
    (with t (db-get-field uid "last-login-address")
      (if (null? t) "" (car t)))))

(tm-define (server-set-user-last-login-address-uid uid t)
  (with-database (server-database)
    (db-set-field uid "last-login-address" (list t))))

(tm-define (server-get-user-failed-login-counter-uid uid)
  (with-database (server-database)
    (with t (db-get-field uid "failed-login-counter")
      (if (null? t) 0 (string->number (car t))))))

(tm-define (server-set-user-failed-login-counter-uid uid t)
  (with-database (server-database)
    (db-set-field uid "failed-login-counter"
		  (list (number->string t)))))

(tm-define (server-get-user-last-failed-login-time-uid uid)
  (with-database (server-database)
    (with t (db-get-field uid "last-failed-login-time")
      (if (null? t) 0 (string->number (car t))))))

(tm-define (server-set-user-last-failed-login-time-uid uid t)
  (with-database (server-database)
    (db-set-field uid "last-failed-login-time"
		  (list (number->string t)))))

(tm-define (server-get-user-last-failed-login-address-uid uid)
  (with-database (server-database)
    (with t (db-get-field uid "last-failed-login-address")
      (if (null? t) "" (car t)))))

(tm-define (server-set-user-last-failed-login-address-uid uid t)
  (with-database (server-database)
    (db-set-field uid "last-failed-login-address" (list t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public server preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-preferences which)
  ;(:synopsis "Set multiple preferences from pairs @which")
  (with changed
        (pair-fold
          (lambda (x prev)
            (let
              ((key (caar x))
               (val (if (string? (cdar x))
                      (cdar x)
              ; (display* (car x) "\n")
                      (if (boolean? (cdar x))
                        (if (cdar x) "on" "off")
                        (object->string (cdar x))))))
              (if (and (!= (get-preference key) val)
                       (!= (get-preference key) "default"))
                (begin
                  (cpp-set-preference key val)
                  (notify-preference key)
                  (cons key prev))
                prev)))
          '()
          which)
        (when (list>0? changed)
          (save-preferences))
        changed))

(tm-service (remote-public-preferences)
  (if (== (get-preference "server service public preferences") "on")
      (server-return envelope (server-public-preferences))
      (server-error envelope "not allowed")))

(tm-service (remote-admin-preferences)
  ; (display* "checking admin preferences fetching allowed\n")
  (if (and (== (get-preference "server service admin preferences") "on")
           (server-check-admin? envelope))
      (server-return envelope
                     (append (server-admin-preferences)
                             (server-get-preferences-documents)))
      (server-error envelope "not allowed")))

(tm-service (remote-set-preferences which)
  (if (and (== (get-preference "server service set preferences") "on")
           (server-check-admin? envelope))
    (receive (prefs docs)
             (list-partition
               which
               (lambda (p)
                 (and (!= (car p) "server mail new-account")
                      (!= (car p) "server mail reset-credentials")
                      (!= (car p) "server license"))))
             (server-return envelope
                            (append (set-preferences prefs)
                                    (server-set-preferences-documents docs))))
    (server-error envelope "not allowed")))

(define server-preferences-edit-form-path
  "$TEXMACS_PATH/progs/forms/server-preferences.tm")

(define (is-on? pref)
  (or (and (boolean? pref) pref)
      (with p (if (string? pref) pref (object->string pref))
        (or (== p "on") (== p "true")))))

(define (get-pref prefs p)
  (ahash-ref (if (list? prefs) (list->ahash-table prefs) prefs) p))

(define (stree-map fun l)
  (cond ((or (null? l) (nlist? l)) l)
        (else (map (lambda (x) (fun x) (stree-map fun x)) l))))

(define (load-preferences-in-stree stree prefs)
  (stree-map
    (lambda (l)
      (case (safe-car l)
        ((form-input-text)
         (and-with pref (get-pref prefs (cadr l)) (list-set! l 5 pref)))
        ((form-text-area)
         (and-with pref (get-pref prefs (cadr l)) (list-set! l 5 pref)))
        ((form-checkbox)
         (and-with pref (get-pref prefs (cadr l))
                   (list-set! l 2 (if (is-on? pref) "true" "false"))))
        (else l)))
    stree))

(define (generate-preferences-form prefs)
  (with tree (tree-import server-preferences-edit-form-path "texmacs")
        (load-preferences-in-stree (tree->stree tree) prefs)))

(tm-service (remote-admin-preferences-form)
  (if (and (== (get-preference "server service admin preferences") "on")
           (server-check-admin? envelope))
    (server-return envelope
                   (generate-preferences-form
                     (append (server-admin-preferences)
                             (server-get-preferences-documents))))
    (server-error envelope "not allowed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; License
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-license-url "$TEXMACS_HOME_PATH/server/license.tm")

(define (generic-document doc)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body ,doc)))

(tm-define (server-get-license-body)
  (tmfile-get
    (if (url-exists? server-license-url)
      (tree-import server-license-url "texmacs")
      (with empty (stree->tree (generic-document '(document "")))
        (tree-export empty server-license-url "texmacs")
        empty)) 'body))

(tm-service (server-license)
  (with s (and (url-exists? server-license-url)
               (string-load server-license-url))
    (with doc (and s (convert s "texmacs-document" "texmacs-stree"))
     (server-return envelope doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mailer configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (server-mailer-instantiate pseudo name email code text)
  (string-replace
   (string-replace
    (string-replace
     (string-replace text "$USER_PSEUDO" pseudo)
     "$USER_NAME" name)
    "$USER_EMAIL" email)
   "$USER_CODE" code))

(tm-define (server-send-email pseudo name email code msg)
  (let* ((cmd0 (server-get-mail-command))
	 (cmd1 (server-mailer-instantiate pseudo name email code cmd0))
	 (msg1 (server-mailer-instantiate pseudo name email code msg))
	 (file (url-temp)))
    (string-save msg1 file)
    (with cmd2 (string-replace cmd1 "$FILE_NAME" (url->string file))
      (server-log-write `info (string-append "running command: " cmd2))
      (server-log-write `info (string-append "where "
					     (url->string file) "= " msg1))
      (with ret (eval-system cmd2)
	(server-log-write `info (string-append "command output: " ret))
	(url-remove file)))))

(define (file-content-equals? f data)
  (and (url-exists? f) (== (string-load f) data)))

(define (server-get-preferences-documents)
  (with docs (make-ahash-table)
        (ahash-set! docs "server mail new-account"
                    (cons 'document
                          (string-split
                            (string-load (server-get-email-new-account))
                            #\newline)))
        (ahash-set! docs "server mail reset-credentials"
                    (cons 'document
                          (string-split
                            (string-load (server-get-email-reset-credentials))
                            #\newline)))
        (ahash-set! docs "server license"
                    (tree->stree (server-get-license-body)))
        (ahash-table->list docs)))

(define (server-set-preferences-documents docs)
  (list-fold
    (lambda (x prev)
      (let ((pref (car x))
            (doc (cdr x)))
      (cond ((and (== pref "server mail new-account")
                  (not (file-content-equals? (server-get-email-new-account)
                                             (cdr doc))))
             (begin
               (string-save (string-join (cdr doc) "\n")
                            (server-get-email-new-account))
               (cons pref prev)))
            ((and (== pref "server mail reset-credentials")
                  (not (file-content-equals?
                         (server-get-email-reset-credentials)
                         (cdr doc))))
             (begin
               (string-save (string-join (cdr doc) "\n")
                            (server-get-email-reset-credentials))
               (cons pref prev)))
            ((and (== pref "server license"))
             (begin
               (tree-export (stree->tree (generic-document doc))
                            server-license-url "texmacs")
               (cons pref prev)))
            (else (begin (format #t "returning prev\n") prev)))))
    '()
    docs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New account email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-email-new-account-source-url
  (string->url "$TEXMACS_PATH/progs/server/server-email-new-account.txt"))

(define server-email-new-account-url
  (string->url "$TEXMACS_HOME_PATH/server/email-new-account.txt"))

(tm-define (server-get-email-new-account)
  (if (not (url-exists? server-email-new-account-url))
      (system-copy server-email-new-account-source-url
		   server-email-new-account-url))
  server-email-new-account-url)

(tm-define (server-send-email-new-account pseudo name email code)
  (server-send-email pseudo name email code
		     (string-load (server-get-email-new-account))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset credentials email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-email-reset-credentials-source-url
  (string->url "$TEXMACS_PATH/progs/server/server-email-reset-credentials.txt"))

(define server-email-reset-credentials-url
  (string->url "$TEXMACS_HOME_PATH/server/email-reset-credentials.txt"))

(tm-define (server-get-email-reset-credentials)
  (if (not (url-exists? server-email-reset-credentials-url))
      (system-copy server-email-reset-credentials-source-url
		   server-email-reset-credentials-url))
  server-email-reset-credentials-url)

(tm-define (server-send-email-reset-credentials pseudo name email code)
  (server-send-email pseudo name email code
		     (string-load (server-get-email-reset-credentials))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-logged-table (make-ahash-table))

(tm-define (server-get-user envelope)
  (with client (car envelope)
    (and client (ahash-ref server-logged-table client))))

(tm-define (server-get-target-user user envelope)
  (if (and (server-check-admin? envelope) user)
    user (server-get-user envelope)))

(tm-define (server-check-admin? envelope)
  (and-with uid (server-get-user envelope)
    (with (pseudo name credentials email admin) (ahash-ref server-users uid)
      admin)))

(tm-define (server-can-login-uid? uid)
  (let ((now (current-time))
	(n (server-get-user-failed-login-counter-uid uid))
	(t (server-get-user-last-failed-login-time-uid uid)))
    (server-log-write `log-notice
      (format #f
          "user ~A: ~A failed logins, ~A last failure, suspended: ~A\n"
          uid n (strftime "%c" (localtime t)) (server-user-suspended? uid)))
    (and (not (server-user-suspended? uid))
	 (or (< n (server-get-failed-login-limit))
	     (> now (+ t (server-get-failed-login-delay)))))))

(tm-define (server-login-uid uid client pseudo)
  (and (server-can-login-uid? uid)
      (server-set-user-last-login-time-uid uid (current-time))
      (server-set-user-last-login-address-uid uid (server-client-address client))
      (server-set-user-failed-login-counter-uid uid 0)
      (ahash-set! server-logged-table client uid)
      (ahash-set! server-logged-table uid client)
      (server-log-write `log-notice (string-append "user " pseudo
	" logged in client " (number->string client)))))

(tm-define (server-failed-login-uid uid client pseudo)
  (ahash-remove! server-logged-table client)
  (ahash-remove! server-logged-table uid)
  (server-set-user-last-failed-login-time-uid uid (current-time))
  (server-set-user-last-failed-login-address-uid
   uid (server-client-address client))
  (server-set-user-failed-login-counter-uid uid
   (+ 1 (server-get-user-failed-login-counter-uid uid)))
  (server-log-write `log-notice (string-append "user " pseudo
     " failed to log at client " (number->string client))))

(tm-define (server-can-login? pseudo)
  (and-with uid (server-find-user pseudo)
    (server-can-login-uid? uid)))

(tm-define (server-login client pseudo)
  (and-with uid (server-find-user pseudo)
    (server-login-uid uid client pseudo)))

(tm-define (server-failed-login client pseudo)
  (and-with uid (server-find-user pseudo)
    (server-failed-login-uid uid client pseudo)))

(tm-define (server-logged? client pseudo)
  (and-with uid (server-find-user pseudo)
    (== (ahash-ref server-logged-table client) uid)))

(tm-define (uid-logged? uid) (ahash-ref server-logged-table uid))
(tm-define (pseudo-logged? pseudo)
  (and-with uid (server-find-user pseudo) (uid-logged? uid)))

;; detect 128 byte salt (old, wrong standard)
(define (server-salt-update salt)
  (if (> (string-length salt) 128) (generate-salt) salt))

(tm-define (server-password-update pseudo passwd)
  (when (server-password-update?)
    (and-with uid (server-find-user pseudo)
      (with (pseudo2 name2 credentials2 email2 admin2)
            (ahash-ref server-users uid)
            (let* ((encoding (server-password-encoding))
                   (credentials (server-credentials-normalize credentials2))
                   (credential-passwords
                     (filter server-credential-password? credentials))
                   (salt (third (car credential-passwords)))
                   (new-salt (server-salt-update salt)))
              (when (or (and (nnull? credential-passwords)
                             (!= (second (car credential-passwords)) encoding))
                        (!= new-salt salt))
                (let* ((hidden (server-password-encode passwd new-salt encoding))
                       (credentials-other
                         (filter
                           (lambda (x) (not (server-credential-password? x)))
                           credentials)))
                  (when hidden
                    (server-set-user-info uid pseudo2 name2
                                          (cons hidden credentials-other)
                                          email2 admin2)
                    (server-log-write
                      `notice
                      (string-append "user " pseudo
                                     " updaded password encoding"))))))))))

(tm-service (remote-login pseudo passwd)
  (if (!= (get-preference "server service login") "on")
      (server-return envelope "not allowed")
      (with uid (server-find-user pseudo)
        (if (not uid)
	    (if (server-pending-pseudo-exists? pseudo)
		(with l (server-find-pending-user pseudo)
		  (server-return envelope "pending"))
		(server-return envelope "user not found"))
            (with (pseudo2 name2 credentials2 email2 admin2)
                (ahash-ref server-users uid)
              ; (display* "remote-login user " pseudo ", with passwd: " passwd "\n")
              (server-log-write `debug
                (format #f "remote-login info: ~A\n" (ahash-ref server-users uid)))
              (when (string? credentials2) ;; Backward compatibility
                (set! credentials2 `((password "clear" ,credentials2))))
              (if (server-password-authentified? pseudo passwd credentials2)
                  (with client (car envelope)
                    (server-password-update pseudo passwd)
                    (server-login-uid uid client pseudo)
                    (server-return envelope "ready"))
                  (with client (car envelope)
		    (if (server-can-login-uid? uid)
			(server-return envelope
			  "invalid password or suspended account")
			(server-return envelope
			  "two many invalid passwords, the account is suspended"))
		    (server-failed-login-uid uid client pseudo))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confirm pending new account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (confirm-pending-account pseudo code)
  (with l (server-find-pending-user pseudo)
    (if (or (not l) (!= code (sixth l)))
	(with client (car envelope)
	  (server-log-write
	   `info (string-append "wrong confirmation code for " pseudo))
	  (if l (server-remove-pending-user pseudo))
	  (server-return envelope "wrong code"))
	(if (server-pseudo-exists? pseudo)
	    (server-return envelope "user already exists")
	    (begin
	      (server-set-user-info
	       #f pseudo (first l) (second l) (third l) (fourth l))
	      (server-remove-pending-user pseudo)
	      (server-return envelope "done"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-logged?)
  (with uid (server-get-user envelope)
    (if (not uid) (server-return envelope "no")
	(server-return envelope "yes"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-logout client pseudo)
  (when (server-logged? client pseudo)
    (client-stop client)
    (and-with uid (ahash-ref server-logged-table client)
      (ahash-remove! server-logged-table client)
      (ahash-remove! server-logged-table uid))
    (server-log-write `log-info (string-append "user " pseudo
      " logged out client " (number->string client)))))

(tm-define (server-logout-client client)
  (and-with uid (ahash-ref server-logged-table client)
    (with (pseudo name credentials email admin)
        (ahash-ref server-users uid)
      (server-logout client pseudo))))

(tm-service (remote-logout)
  (let* ((client (car envelope))
         (uid (ahash-ref server-logged-table client)))
    (if (not uid) (server-error envelope "user not logged")
        (with (pseudo name credentials email admin)
            (ahash-ref server-users uid)
          (server-logout client pseudo)
          (server-return envelope "bye")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account informations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (credential->authentication credential)
  (cond ((string? credential) `password)
	((and (list? credential) (nnull? credential)) (car credential))
	(else (begin (server-log-write `error "wrong credential detected"))
	      `unknown)))

(define (credentials->authentications credentials)
  (if (list? credentials)
      (map credential->authentication credentials)
      (list (credential->authentication credentials))))

(tm-service (remote-get-account user)
  (with uid (server-get-target-user user envelope)
    (if (not uid)
      (server-error envelope "user not logged")
      (server-return envelope (user-info->list uid)))))

(tm-service (remote-set-account user info)
  (with uid (server-get-target-user user envelope)
    (if (not uid) (server-return envelope "user not logged")
        (with (pseudo name credentials email admin)
            (ahash-ref server-users uid)
          (for (x info)
            (when (list-2? x)
              (cond
                ((== (first x) "name") (set! name (second x)))
                ((== (first x) "email") (set! email (second x)))
                ((and (== (first x) "credentials") (list>0? (second x)))
		 (begin
		   (if (and (!= (get-preference
				 "server require strong passwords") "off")
			    (not (server-strong-credentials? (second x))))
		       (set! credentials #f)
		       (set! credentials
			     (server-credentials-update
			      credentials (server-add-salt
                                            (second x)
                                            (generate-salt))))))))))
	  (if credentials
	      (begin
		(server-set-user-information
		 pseudo name credentials email (if admin "yes" "no"))
		(server-return envelope "done"))
	      (server-return envelope server-strong-password-instructions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset credentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-reset-credentials-users (make-ahash-table))

(define (server-load-reset-credentials-users)
  (when (== (ahash-size server-reset-credentials-users) 0)
    (with f "$TEXMACS_HOME_PATH/server/reset-credentials-users.scm"
      (set! server-reset-credentials-users
            (if (url-exists? f)
                (list->ahash-table (load-object f))
                (make-ahash-table))))))

(define (server-save-reset-credentials-users)
  (with f "$TEXMACS_HOME_PATH/server/reset-credentials-users.scm"
    (save-object f (ahash-table->list server-reset-credentials-users))))

(tm-define (server-clean-reset-credentials-users)
  (with d (server-get-reset-credentials-delay)
    (when (>= d 0)
      (server-load-reset-credentials-users)
      (let* ((l (map car (ahash-table->list server-reset-credentials-users)))
	     (t (- (current-time) d)))
	(for (pseudo l)
	  (with u (ahash-ref server-reset-credentials-users pseudo)
	    (when (< (string->number (first u)) t)
	      (server-log-write `log-notice
	       (string-append "removing reset-credentials user " pseudo))
	      (ahash-remove! server-reset-credentials-users pseudo)))))
      (server-save-reset-credentials-users))))

(tm-define (server-find-reset-credentials-user pseudo)
  (server-load-reset-credentials-users)
  (ahash-ref server-reset-credentials-users pseudo))

(tm-define (server-get-reset-credentials-code pseudo)
  (and-with l (server-find-reset-credentials-user pseudo)
    (server-remove-reset-credentials-user pseudo) ;; the code can be used once
    (second l)))

(tm-define (server-create-reset-credentials-user pseudo)
  (server-load-reset-credentials-users)
  (with code (number->string (random 1000000))
    (when (not (server-find-reset-credentials-user pseudo))
      (ahash-set! server-reset-credentials-users pseudo
		  (list (number->string (current-time)) code))
      (server-save-reset-credentials-users))
    code))

(tm-define (server-remove-reset-credentials-user pseudo)
  (server-load-reset-credentials-users)
  (when (server-find-reset-credentials-user pseudo)
    (ahash-remove! server-reset-credentials-users pseudo)
    (server-save-reset-credentials-users)))

(tm-service (remote-reset-credentials pseudo)
  (with uid (server-find-user pseudo)
    (if (or (not uid)
	    (!= (get-preference "server service reset-credentials") "on")
	    (not (server-can-login-uid? uid)))
	(server-return envelope "not allowed")
	(with (pseudo name credentials email admin)
	    (ahash-ref server-users uid)
	  (with code (server-create-reset-credentials-user pseudo)
	    (server-send-email-reset-credentials pseudo name email code))
	(server-return envelope "done")))))

(tm-service (remote-login-code pseudo code)
  (if (!= (get-preference "server service reset-credentials") "on")
      (server-return envelope "not allowed")
      (with uid (server-find-user pseudo)
        (if (not uid)
	    (server-return envelope "user not found")
	    (with client (car envelope)
	      (with code2 (server-get-reset-credentials-code pseudo)
		(if (== code code2)
		    (begin
		      (server-login-uid uid client pseudo)
		      (server-return envelope "ready"))
		    (begin
		      (server-log-write
		       `info (string-append "invalid code for " pseudo))
		      (server-return envelope "invalid code")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-eval cmd)
  (if (server-check-admin? envelope)
      (with ret (eval cmd)
        (when (debug-get "remote")
          (display* "server-remote-eval " cmd " -> " ret "\n"))
        (server-return envelope ret))
      (server-error envelope "execution of commands is not allowed")))
