
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-widgets.scm
;; DESCRIPTION : widgets for remote clients
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-widgets)
  (:use (client client-tmfs)
        (client client-sync)
        (client client-chat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((error-widget s) quit)
  (padded
    (centered (text s))
    ======
    (bottom-buttons
      >> ("Ok" (quit)) >>)))

(tm-define (open-error s)
  (:interactive #t)
  (dialogue-window (error-widget s) noop "Error"))

(define (first-incomplete-field t pw? key?)
  (cond ((== (ahash-ref t "server") "") "Server")
	((== (ahash-ref t "pseudo") "") "Pseudo")
	((== (ahash-ref t "name") "") "Full name")
	((== (ahash-ref t "email") "") "Email")
	((and pw? (== (ahash-ref t "password") "")) "Password")
	((and pw? (== (ahash-ref t "repeat") "")) "Repeat")
	(else #f)))

(tm-widget (remote-account-widget quit)
  (let* ((use-password? #t)
	 (use-key? #f)
	 (set-password?
	  (lambda (flag?)
	    (set! use-password? flag?)
	    (refresh-now "password-info")))
	 (set-key?
	  (lambda (flag?)
	    (set! use-key? flag?)
	    (refresh-now "key-info"))))
    (padded
      (form "account-info"
	(aligned
	  (item (text "Server:")
	    (form-input "server" "string"
			(list "") "300px"))
	  (item (text "Pseudo:")
	    (form-input "pseudo" "string"
			(list (get-user-info "pseudo")) "300px"))
	  (item (text "Full name:")
	    (form-input "name" "string"
			(list (get-user-info "name")) "300px"))
	  (item (text "Email:")
	    (form-input "email" "string"
			(list (get-user-info "email")) "300px")))
	====== ======
	(aligned
	  (item (toggle (set-password? answer) use-password?)
	    (hlist
	      // (text "Allow authentification through password"))))
	===
	(refreshable "password-info"
	  (when use-password?
	    (aligned
	      (item (text "Password:")
		(form-input "password" "password" (list "") "300px"))
	      (item (text "Repeat:")
		(form-input "repeat" "password" (list "") "300px")))))
	====== ======
	(when #f
	  (aligned
	    (item (toggle (set-key? answer) use-key?)
	      (hlist
		// (text "Allow authentification through cryptographic key")))))
	===
	(refreshable "key-info"
	  (when use-key?
	    (text "Not yet implemented")))
	======
	(bottom-buttons
	  >>
	  ("Proceed"
	   (with t (make-ahash-table)
	     (for-each (cut ahash-set! t <> <>) (form-fields) (form-values))
	     (cond ((not (or use-password? use-key?))
		    (open-error
		     "Please enable at least one authentification method"))
		   ((and use-password? (!= (ahash-ref t "password")
					   (ahash-ref t "repeat")))
		    (open-error "Passwords do not match"))
		   ((first-incomplete-field t use-password? use-key?) =>
		    (lambda (s)
		      (open-error (string-append "Missing '" s "'"))))
		   (else
		     (client-create-account (ahash-ref t "server")
					    (ahash-ref t "pseudo")
					    (ahash-ref t "name")
					    (ahash-ref t "password")
					    (ahash-ref t "email"))
		     (quit))))))))))

(tm-define (open-remote-account-creator)
  (:interactive #t)
  (dialogue-window remote-account-widget noop "Create remote account"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create account after licence agreement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((save-licence-as doc) f)
  (with s (convert doc "texmacs-stree" "texmacs-document")
    (string-save s f)))

(tm-widget ((accept-licence-widget doc) cmd)
  (let* ((ok? #f)
	 (decl (string-append "I declare having read and agreed with "
			      "the above licence agreement"))
	 (msg (string-append "You must agree with the licence "
			     "in order to proceed")))
    (padded
      ======
      (resize "720px" "480px"
	(texmacs-output `(with "bg-color" "#fff" ,(tmfile-extract doc 'body))
			`(style ,(tmfile-extract doc 'style))))
      ======
      (hlist
	(toggle (set! ok? answer) ok?) // // //
	(text decl) >>)
      ======
      (bottom-buttons
	>>
	("Save" (choose-file (save-licence-as doc) "Save licence" "texmacs"))
	// // //
	("Cancel" (cmd #f))
	// // //
	("Ok" (if ok? (cmd #t) (open-error msg)))))))

(tm-define (client-create-account server-name pseudo name passwd email)
  (with cmd (lambda ()
	      (client-new-account server-name pseudo name passwd email ""))
    (with server (client-start server-name)
      (when (!= server -1)
	(client-remote-eval server `(server-licence)
	  (lambda (doc)
	    (client-stop server)
	    (if (not doc) (cmd)
		(dialogue-window (accept-licence-widget doc)
				 (lambda (accept?)
				   (if accept? (cmd)))
				 "Server licence agreement"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-login-home server-name pseudo passwd)
  (client-login-then
   server-name pseudo passwd
   (lambda (ret)
     (when (== ret "ready")
       (with-wallet
         (wallet-set (list "remote" server-name pseudo) passwd))
       (and-let* ((server (client-find-server server-name))
                  (dir (remote-home-directory server)))
         (load-document dir)))
     (when (== ret "invalid password")
       (with server (client-find-server server-name)
         (client-logout server))
       (dialogue-window (remote-login-widget server-name pseudo #t)
                        noop "Remote login")))))

(tm-widget ((remote-login-widget server-name pseudo retry?) quit)
  (padded
    (form "login-info"
      (if retry?
          ===
          (centered (bold (text "Incorrect password; please try again."))))
      ======
      (aligned
	(item (text "Server:")
	  (form-input "server" "string"
		      (list server-name) "300px"))
	(item (text "Pseudo:")
	  (form-input "pseudo" "string"
		      (list pseudo) "300px"))
	(item (text "Password:")
	  (form-input "name" "password"
		      (list "") "300px")))
      ======
      (bottom-buttons
	>>
        ("Cancel" (quit)) // //
	("Ok" (apply client-login-home (form-values)) (quit))))))

(tm-define (open-remote-login server-name pseudo)
  (:interactive #t)
  (with-wallet
    (with passwd (wallet-get (list "remote" server-name pseudo))
      (if passwd
          (client-login-home server-name pseudo passwd)
          (dialogue-window (remote-login-widget server-name pseudo #f)
                           noop "Remote login")))))

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

(tm-define (open-file-permissions-editor server u)
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
      ("Browse"
       (when (or (remote-file-name name) (remote-home-directory server))
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
	(tab (text "Data")
	  (dynamic (client-auto-sync-data-widget server)))
	(tab (text "Files")
	  (dynamic (client-auto-sync-files-widget server))))
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

(tm-widget ((upload-widget server) quit)
  (padded
    (form "upload-form"
      ======
      (aligned
        (item (text "Local source:")
	  (with dummy (form-set "local-name"
                                (if (remote-file-name (current-buffer)) ""
                                    (url->system (current-buffer))))
	    (dynamic (form-local-widget "upload-form" "local-name"
					"Local file or directory"
					"" "300px"))))
        (item (text "Remote destination:")
	  (with dummy (form-set "remote-name"
                                (if (remote-file-name (current-buffer))
                                    (url->system (current-buffer)) ""))
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
		   (remote-upload local-name remote-name message)))
	     (quit))))))))

(tm-define (remote-interactive-upload server)
  (:interactive #t)
  (dialogue-window (upload-widget server) noop
		   "Upload file or directory"))

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
