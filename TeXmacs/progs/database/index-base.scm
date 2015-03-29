
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
;; Counter management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-get-counter table var val)
  (with l (db-sql* "SELECT count FROM " table " WHERE"
                   " " var "=" (sql-quote val))
    (if (null? l) 0 (string->number (car l)))))

(define (index-increase-counter table var val)
  (with nr (index-get-counter table var val)
    (if (== nr 0)
        (db-sql "INSERT INTO " table " VALUES (" (sql-quote val) ", 1)")
        (db-sql "UPDATE " table
                " SET count=" (number->string (+ nr 1)) " WHERE"
                " " var "=" (sql-quote val)))))

(tm-define (index-number-completions prefix)
  (index-get-counter "prefixes_count" "prefix" prefix))

(tm-define (index-number-name-completions prefix)
  (index-get-counter "completions_count" "prefix" prefix))

(tm-define (index-number-matches key)
  (index-get-counter "matches_count" "key" key))

(tm-define (index-number-hits key)
  (index-get-counter "scores_count" "key" key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building prefix tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define index-prefixes-done (make-ahash-table))

(define (index-get-prefixes table key)
  (db-sql* "SELECT DISTINCT prefix FROM " table " WHERE"
           " key=" (sql-quote key)))

(define (index-get-completions* table prefix)
  (db-sql* "SELECT DISTINCT key FROM " table " WHERE"
           " prefix=" (sql-quote prefix)))

(define (index-insert-prefixes table key)
  (let* ((key* (list table key))
         (cnt (string-append table "_count")))
    (when (not (ahash-ref index-prefixes-done key*))
      (with r (index-get-prefixes table key)
        (when (null? r)
          (for (i (... 1 (string-length key)))
            (with prefix (substring key 0 i)
              (db-sql "INSERT INTO " table " VALUES (" (sql-quote prefix)
                      ", " (sql-quote key) ")")
              (index-increase-counter cnt "prefix" prefix))))
        (ahash-set! index-prefixes-done key* #t)))))

(tm-define (index-get-completions prefix)
  (index-get-completions* "prefixes" prefix))

(tm-define (index-get-name-completions prefix)
  (index-get-completions* "completions" prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building matches and scores tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-set-matches id attr keys)
  (db-sql "DELETE FROM matches WHERE"
          " id=" (sql-quote id) " AND"
          " attr=" (sql-quote attr))
  (for (key keys)
    (index-insert-prefixes "prefixes" key)
    (db-sql "INSERT INTO matches VALUES (" (sql-quote id)
            ", " (sql-quote attr)
            ", " (sql-quote key) ")")
    (index-increase-counter "matches_count" "key" key)))

(define (index-set-scores id scores)
  (db-sql "DELETE FROM scores WHERE"
          " id=" (sql-quote id))
  (for (key-score scores)
    (with (key score) key-score
      (index-insert-prefixes "prefixes" key)
      (db-sql "INSERT INTO scores VALUES (" (sql-quote id)
              ", " (sql-quote key)
              ", " score ")")
      (index-increase-counter "scores_count" "key" key))))

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
    (index-set-matches id attr keys)
    (when (== attr "name")
      (for (name vals)
        (index-insert-prefixes "completions" name)))))

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
  (<= (index-number-completions p) 20))

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
