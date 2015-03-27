
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
        (database db-edit)
        (database db-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-search-cache (make-ahash-table))

(define ((db-search-match? query) entry)
  (and-with name (db-entry-ref entry "name")
    (and (tm-atomic? name)
	 (string-starts? (tm->string name) query))))

(define (db-search-results db kind query)
  (let* ((l (ahash-ref db-search-cache (url->string db)))
	 (f (list-filter l (db-search-match? query)))
	 (h (if (< (length f) 25) f (sublist f 0 25)))
	 (r (db-pretty h kind :compact)))
    (cond ((null? r) (list "No matching items"))
	  ((>= (length r) 25) (rcons r "More items follow"))
	  (else r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-search-keypress db kind event old-query)
  (when (pair? event)
    (with (new-query key) event
      (buffer-set-body "tmfs://aux/db-search-results"
		       `(document ,@(db-search-results db kind new-query)))
      new-query)))

(tm-widget ((db-search-widget db kind) quit)
  (padded
    (with query ""
      (hlist
	(text "Search") // //
	(input (set! query (db-search-keypress db kind answer query))
	       "search-database" (list "") "650px"))
      === ===
      (refreshable "db-search-results"
	(resize "750px" "500px"
	  (texmacs-input `(document ,@(db-search-results db kind query))
			 `(style (tuple ,(db-get-style kind)))
			 "tmfs://aux/db-search-results"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-database-search db kind name)
  (:interactive #t)
  (ahash-set! db-search-cache (url->string db)
	      (with-database db
		(db-load-types (smart-ref db-kind-table kind))))
  (dialogue-window (db-search-widget db kind) noop name))
