
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
;; Pretty printing with cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-pretty-cache (make-ahash-table))

(define (db-pretty-cached l kind fm)
  (when (not (ahash-ref db-pretty-cache (list kind fm)))
    (ahash-set! db-pretty-cache (list kind fm) (make-ahash-table)))
  (with cache (ahash-ref db-pretty-cache (list kind fm))
    (let* ((todo (list-filter l (negate (cut ahash-ref cache <>))))
           (pretty (db-pretty todo kind fm))
           (srcs (make-ahash-table))
           (objs (make-ahash-table)))
      (for-each (lambda (entry)
                  (and-with name (db-entry-ref entry "name")
                    (ahash-set! srcs name entry))) todo)
      (for-each (lambda (p)
                  (and-with name (and (tm-func? p 'bib-result 2) (tm-ref p 0))
                    (ahash-set! objs name p))) pretty)
      (for (name (map car (ahash-table->list srcs)))
        (let* ((src (ahash-ref srcs name))
               (obj (ahash-ref objs name)))
          (when (and src obj)
            (ahash-set! cache src obj)))))
    (let* ((r (map (cut ahash-ref cache <>) l))
           (fr (list-filter r identity)))
      (sort fr (lambda (p1 p2)
                 (and (tm-func? p1 'bib-result 2)
                      (tm-func? p2 'bib-result 2)
                      (string? (tm-ref p1 0))
                      (string? (tm-ref p2 0))
                      (string<=? (tm-ref p1 0) (tm-ref p2 0))))))))

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
	 (h (if (< (length f) 20) f (sublist f 0 20)))
	 (r (db-pretty-cached h kind :compact)))
    (cond ((null? r) (list "No matching items"))
	  ((>= (length r) 20) (rcons r "More items follow"))
	  (else r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-quit-search ignore)

(define (db-search-keypress db kind event old-query)
  (when (pair? event)
    (with (new-query key) event
      (buffer-set-body "tmfs://aux/db-search-results"
		       `(document ,@(db-search-results db kind new-query)))
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

(tm-define (open-db-chooser db kind name call-back)
  (:interactive #t)
  (ahash-set! db-search-cache (url->string db)
	      (with-database db
		(db-load-types (smart-ref db-kind-table kind))))
  (dialogue-window (db-search-widget db kind)
		   (lambda args
		     (set! db-quit-search ignore)
		     (apply call-back args))
		   name))
