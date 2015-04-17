
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
  (:use (client client-tmfs)))

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
  (:argument server-name "Server")
  (:argument pseudo "User pseudo")
  (:argument passwd "password" "Password")
  (display* "Login " server-name ", " pseudo ", " passwd "\n")
  (client-login-then server-name pseudo passwd
                     (lambda (ret)
                       (with server (client-find-server server-name)
                         (load-buffer (remote-home-directory server))))))

(tm-widget ((remote-login-widget server-name pseudo) quit)
  (padded
    (form "login-info"
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
	("Ok"
	 (apply client-login-home (form-values))
	 (quit))))))

(tm-define (open-remote-login server-name pseudo)
  (:interactive #t)
  (dialogue-window (remote-login-widget server-name pseudo) noop
		   "Remote login"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-ancestors dir)
  (let* ((parent (remote-parent dir))
         (this (url->string (url-tail dir)))
         (last (cons this dir)))
    (if (not (remote-home-directory? dir))
        (rcons (get-ancestors parent) last)
        (list last))))

(define (remote-relative dir name dir?)
  (if (remote-root-directory? dir)
      (let* ((root (remote-file-name dir))
             (protocol (if dir? "tmfs://remote-dir/" "tmfs://remote-file/"))
             (full (string-append protocol root)))
        (url-append full name))
      (remote-relative (remote-parent dir) name dir?)))

(tm-widget ((remote-file-browser server orig type) quit)
  (let* ((save-flag? (or (func? type :save-file 1)
                         (func? type :save-directory 1)))
         (dir-flag? (or (== type :directory)
                        (func? type :save-directory 1)))
         (dir (if save-flag? (remote-parent orig) orig))
         (file (and save-flag? (url->string (url-tail orig))))
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

(tm-define (open-remote-file-browser server dir type name cmd)
  (:interactive #t)
  (dialogue-window (remote-file-browser server dir type) cmd name))

(tm-define (remote-rename-interactive server)
  (:interactive #t)
  (with dir? (remote-directory? (current-buffer))
    (open-remote-file-browser
     server (current-buffer)
     (list (if dir? :save-directory :save-file) "Rename as:")
     (if dir? "Rename directory" "Rename file")
     (lambda (new-name)
       (when (url? new-name)
         (remote-rename (current-buffer) new-name))))))

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
