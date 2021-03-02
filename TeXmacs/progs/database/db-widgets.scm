
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-widgets.scm
;; DESCRIPTION : widgets for searching and managing databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-widgets)
  (:use (database db-convert)
        (database db-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("auto bib import" "on" ignore))

(tm-widget (db-preferences-widget)
  (padded
      (aligned
        (meti (hlist // (text "Automatically import bibliographies when opening files") >>)
          (toggle (set-boolean-preference "auto bib import" answer)
                  (get-boolean-preference "auto bib import"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty printing with cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-pretty-cache (make-ahash-table))

(define (get-name entry)
  (or (and (tm-func? entry 'db-result 2)
           (string? (tm-ref entry 0))
           (tm-ref entry 0))
      (db-entry-ref entry "name")))

(define (db-pretty-cached l kind fm)
  (when (not (ahash-ref db-pretty-cache (list kind fm)))
    (ahash-set! db-pretty-cache (list kind fm) (make-ahash-table)))
  (with cache (ahash-ref db-pretty-cache (list kind fm))
    (let* ((todo (list-filter l (negate (cut ahash-ref cache <>))))
           (pretty (db-pretty todo kind fm))
           (srcs (make-ahash-table))
           (objs (make-ahash-table)))
      (for-each (lambda (entry)
                  (and-with name (get-name entry)
                    (ahash-set! srcs name entry))) todo)
      (for-each (lambda (p)
                  (and-with name (get-name p)
                    (ahash-set! objs name p))) pretty)
      (for (name (map car (ahash-table->list srcs)))
        (let* ((src (ahash-ref srcs name))
               (obj (ahash-ref objs name)))
          (when (and src obj)
            (ahash-set! cache src obj)))))
    (with cv (lambda (x) (with y (ahash-ref cache x) (or y x)))
      (sort (map cv l)
            (lambda (p1 p2)
              (let* ((n1 (get-name p1))
                     (n2 (get-name p2)))
                (and n1 n2 (string<=? n1 n2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-search-cache (make-ahash-table))
(define db-result-cache (make-ahash-table))

(define (db-search-cached q)
  (with cached (ahash-ref db-search-cache q)
    (or cached
        (with r (db-search q)
          (ahash-set! db-search-cache q r)
          r))))

(define (db-get-result-cached id)
  (with cached (ahash-ref db-result-cache id)
    (or cached
        (with r (db-load-entry id)
          (ahash-set! db-result-cache id r)
          r))))

(define (db-search-results db kind query)
  (with-database db
    (with-limit 20
      ;; TODO: filter on user permissions
      (let* ((types (smart-ref db-kind-table kind))
	     (q (list (list :completes query)
		      (cons "type" types)))
	     (ids (db-search-cached q))
	     (l (map db-get-result-cached ids))
	     (r (db-pretty-cached l kind :pretty)))
	(cond ((null? r) (list "No matching items"))
	      ((>= (length r) 20) (rcons r "More items follow"))
	      (else r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-quit-search ignore)
(define db-search-keypress-serial 0)

(define (db-search-results-buffer)
  (string->url "tmfs://aux/db-search-results"))

(define (db-search-keypress db kind event old-query)
  (when (pair? event)
    (with (new-query key) event
      (when (!= new-query old-query)
        (with serial (+ db-search-keypress-serial 1)
          (set! db-search-keypress-serial serial)
          (delayed
            (:pause 200)
            (when (== db-search-keypress-serial serial)
              (with doc `(document ,@(db-search-results db kind new-query))
                (buffer-set-body "tmfs://aux/db-search-results" doc))
              ;;(refresh-now "db-search-results")
              ))))
      new-query)))

(tm-define (db-confirm-result t)
  (:secure #t)
  (when (tm-atomic? t)
    (db-quit-search (tm->string t))))

(tm-widget ((db-search-widget db kind) quit)
  (padded
    (let* ((dummy (set! db-quit-search quit))
	   (query ""))
      (hlist
	(text "Search:") // //
	(input (set! query (db-search-keypress db kind answer query))
	       "search-database" (list "") "650px"))
      === ===
      (refreshable "db-search-results"
	(resize "750px" "500px"
	  (texmacs-input `(document ,@(db-search-results db kind query))
			 `(style (tuple ,(db-get-style kind)))
			 (db-search-results-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage user identities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define adding-user? #f)

(define (get-default-pseudo*)
  (if adding-user? "?" (user->pseudo (get-default-user))))

(define (get-user-info* attr)
  (if adding-user? "" (get-user-info attr)))

(define (refresh-identities flag?)
  (refresh-now "identities-list")
  (refresh-now "identity-info")
  (refresh-now "identity-buttons")
  (when (and (in-database?) flag?)
    (revert-buffer-revert)))

(define (set-identity vars vals)
  (if adding-user?
      (with t (make-ahash-table)
        (for-each (cut ahash-set! t <> <>) vars vals)
        (let* ((pseudo (ahash-ref t "pseudo"))
               (name (ahash-ref t "name")))
          (when (and (!= pseudo "") (!= name ""))
            (with uid (add-user pseudo name)
              (set-default-user uid)
              (for-each set-user-info vars vals)
              (set! adding-user? #f)))))
      (for-each set-user-info vars vals))
  (refresh-identities #t))

(tm-widget (delete-user-widget quit)
  (padded
    (centered (text "Really delete user identity?"))
    (centered (bold (text "All data attached to this identity will be lost")))
    ======
    (hlist
      (explicit-buttons
        >>
        ("Cancel" (quit)) // // //
        ("Ok"
         (remove-user)
         (refresh-identities #t)
         (quit))
        >>))))

(tm-widget (db-identities-widget)
  (padded
    ======
    (hlist
      (resize "150px" "250px"
        (vlist
          ;;(bold (text "User"))
          ;;======
          (refreshable "identities-list"
            (choice (begin
                      (set! adding-user? #f)
                      (set-default-user (pseudo->user answer))
                      (refresh-identities #t))
                    (sort (map user->pseudo (get-users-list)) string<=?)
                    (get-default-pseudo*)))))
      // // //
      (resize "375px" "250px"
        (vlist
          ;;(bold (text "Information"))
          ;;======
          (refreshable "identity-info"
            (form "id-info"
              (aligned
                (item (text "Pseudo:")
                  (form-input "pseudo" "string"
                              (list (get-user-info* "pseudo")) "300px"))
                (item (text "Full name:")
                  (form-input "name" "string"
                              (list (get-user-info* "name")) "300px"))
                (item (text "Email:")
                  (form-input "email" "string"
                              (list (get-user-info* "email")) "300px"))
		(item (text "GnuPG key:")
		  (hlist (when (and (== (get-preference
					 "experimental encryption") "on")
				    (supports-gpg?))
			   (with key (get-user-info "gpg-key-fingerprint")
			     (text (if (== key "") ""
				       (string-take-right key 8))))
			   >> ((icon "tm_add.xpm") (open-gpg-key-manager))))))
              (glue #f #t 0 0)
              (hlist
                (explicit-buttons
                  >>
                  ("Set" (set-identity (form-fields) (form-values))))))))))
    ===
    (refreshable "identity-buttons"
      (hlist
        ((icon "tm_add_2.xpm")
         (set! adding-user? #t)
         (refresh-identities #f))
        (if (or adding-user? (> (length (get-users-list)) 1))
            ((icon "tm_close_tool.xpm")
             (if adding-user?
                 (begin
                   (set! adding-user? #f)
                   (refresh-identities #f))
                 (dialogue-window delete-user-widget noop
                                  "Delete user identity"))))
        >>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-identities)
  (:interactive #t)
  (set! adding-user? #f)
  (top-window db-identities-widget "Specify user identity"))

(tm-define (open-db-chooser db kind name call-back)
  (:interactive #t)
  (db-reset)
  (set! db-search-cache (make-ahash-table))
  (set! db-result-cache (make-ahash-table))
  (dialogue-window (db-search-widget db kind)
		   (lambda args
		     (set! db-quit-search ignore)
		     (apply call-back args))
		   name (db-search-results-buffer)))

(tm-define (open-db-preferences)
  (:interactive #t)
  (top-window db-preferences-widget "TeXmacs database preferences"))
