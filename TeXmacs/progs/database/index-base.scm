
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : index-base.scm
;; DESCRIPTION : Indexing TeXmacs databases and file systems
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database index-base)
  (:use (database db-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current indexation method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define db-indexing #f)

(tm-define-macro (with-indexing ind . body)
  `(with-global db-indexing ,ind ,@body))

(tm-define (db-reset)
  (former)
  (set! db-indexing #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building prefix table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define index-prefixes-done (make-ahash-table))

(define (index-get-prefixes key)
  (db-sql* "SELECT DISTINCT prefix FROM prefixes WHERE"
           " key=" (sql-quote key)))

(define (index-get-completions prefix)
  (with l (if db-limit (string-append " LIMIT " (number->string db-limit)) "")
    (db-sql* "SELECT DISTINCT key FROM prefixes WHERE"
             " prefix=" (sql-quote prefix) l)))

(define (index-insert-prefixes key)
  (when (not (ahash-ref index-prefixes-done key))
    (with r (index-get-prefixes key)
      (when (null? r)
        (for (i (... 1 (string-length key)))
          (with prefix (substring key 0 i)
            (db-sql "INSERT INTO prefixes VALUES (" (sql-quote prefix)
                    ", " (sql-quote key) ")"))))
      (ahash-set! index-prefixes-done key #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building matches and scores tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-set-matches id attr keys)
  (db-sql "DELETE FROM matches WHERE"
          " id=" (sql-quote id) " AND"
          " attr=" (sql-quote attr))
  (for (key keys)
    (index-insert-prefixes key)
    (db-sql "INSERT INTO matches VALUES (" (sql-quote id)
            ", " (sql-quote attr)
            ", " (sql-quote key) ")")))

(define (index-set-scores id scores)
  (db-sql "DELETE FROM scores WHERE"
          " id=" (sql-quote id))
  (for (key-score scores)
    (with (key score) key-score
      (index-insert-prefixes key)
      (db-sql "INSERT INTO scores VALUES (" (sql-quote id)
              ", " (sql-quote key)
              ", " score ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routines for indexation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-get-keys vals)
  ;; TODO: take into account encodings of field values
  (let* ((l (list-intersperse vals "\n"))
         (s (apply string-append l)))
    (compute-keys-string s "verbatim")))

(tm-define (index-indexate-field id attr vals)
  (with keys (index-get-keys vals)
    (index-set-matches id attr keys)))

(tm-define (index-indexate-entry id l)
  (for (f l)
    (with (attr . vals) f
      (index-indexate-field id attr vals))))

(define (index-get-scores u)
  (with l (compute-index-url u)
    (map (lambda (t) (map object->string t)) l)))

(tm-define (index-indexate-file id u)
  (with scores (index-get-scores u)
    (index-set-scores id scores)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforming a search string into a list of queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string->queries q)
  (let* ((keys (compute-keys-string q "verbatim"))
         (keys* (list-filter keys (lambda (s) (>= (string-length s) 2))))
         (longer? (lambda (s1 s2) (>= (string-length s1) (string-length s2))))
         (l (sort keys* longer?)))
    (map (lambda (s) (list :match s)) l)))

(define (admissible-prefix? p)
  (or (not (integer? db-limit))
      (< (length (index-get-completions p)) db-limit)))

(tm-define (prefix->queries q)
  (let* ((keys (compute-keys-string q "verbatim"))
         (keys* (list-filter keys (lambda (s) (>= (string-length s) 2))))
         (longer? (lambda (s1 s2) (>= (string-length s1) (string-length s2)))))
    (if (null? keys*) (list)
        (let* ((l1 (sort (cDr keys*) longer?))
               (l2 (list-filter (list (cAr keys*)) admissible-prefix?)))
          (append (map (lambda (s) (list :prefix s)) l2)
                  (map (lambda (s) (list :match s)) l1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapping the basic database API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-set-field id attr vals)
  (if (not db-indexing)
      (former id attr vals)
      (with-indexing #f
        (former id attr vals)
        (index-indexate-field id attr vals))))

(tm-define (db-set-entry id l)
  (if (not db-indexing)
      (former id l)
      (with-indexing #f
        (former id l)
        (index-indexate-entry id l))))

(tm-define (db-search-table query i)
  (:require (and db-indexing (func? query :match 1)))
  (string-append "matches AS p" (number->string i)))

(tm-define (db-search-table query i)
  (:require (and db-indexing (func? query :prefix 1)))
  (string-append "prefixes AS pre" (number->string i) " JOIN "
                 "matches AS p" (number->string i)))

(tm-define (db-search-constraint query i)
  (:require (and db-indexing (func? query :match 1)))
  (let* ((key (cadr query))
         (pi (string-append "p" (number->string i))))
    (string-append pi ".key=" (sql-quote key))))

(tm-define (db-search-constraint query i)
  (:require (and db-indexing (func? query :prefix 1)))
  (let* ((prefix (cadr query))
         (pi (string-append "p" (number->string i)))
         (prei (string-append "pre" (number->string i))))
    (string-append prei ".prefix=" (sql-quote prefix) " AND "
                   pi ".key=" prei ".key")))
