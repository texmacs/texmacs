
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

(define (get-name entry)
  (or (and (tm-func? entry 'bib-result 2)
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

(define (db-search-results db kind query)
  (with-database db
    (with-indexing #t
      ;; TODO: filter on user permissions
      (let* ((types (smart-ref db-kind-table kind))
             (q (append (string->queries query)
                        (list (cons "type" types))))
             (ids (db-search q))
             (ids* (if (< (length ids) 20) ids (sublist ids 0 20)))
             (l (map db-load-entry ids*))
             (r (db-pretty-cached l kind :compact)))
        (cond ((null? r) (list "No matching items"))
              ((>= (length r) 20) (rcons r "More items follow"))
              (else r))))))

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
	(text "Search:") // //
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
  (db-reset)
  (dialogue-window (db-search-widget db kind)
		   (lambda args
		     (set! db-quit-search ignore)
		     (apply call-back args))
		   name))
