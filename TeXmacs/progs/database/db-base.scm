
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-base.scm
;; DESCRIPTION : TeXmacs databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution of SQL commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define global-database
  (url-concretize "$TEXMACS_HOME_PATH/server/global.tmdb"))

(tm-define current-database (url-none))

(tm-define-macro (with-database db . body)
  `(with-global current-database ,db ,@body))

(tm-define (db-init-database)
  (when (url-none? current-database)
    (texmacs-error "db-init-database" "no database specified"))
  (when (not (url-exists? current-database))
    (display* "Create " current-database "\n")
    (sql-exec current-database
              (string-append "CREATE TABLE props ("
                             "id text, attr text, val text, "
			     "created integer, expires integer)"))))

(tm-define (db-sql . l)
  (db-init-database)
  (display* (url-tail current-database) "] " (apply string-append l) "\n")
  (sql-exec current-database (apply string-append l)))

(tm-define (db-sql* . l)
  (with r (apply db-sql l)
    (with f (lambda (x) (and (pair? x) (car x)))
      ;; NOTE: sqlite3 delivers results in reverse ordering
      (reverse (map f (if (null? r) r (cdr r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define db-time "strftime('%s','now')")
(tm-define db-time-stamp? #f)
(tm-define db-extra-fields (list))

(tm-define-macro (with-time t . body)
  `(with-global db-time (db-decode-time ,t) ,@body))

(tm-define-macro (with-time-stamp on? . body)
  `(with-global db-time-stamp? on? ,@body))

(tm-define-macro (with-extra-fields l . body)
  `(with-global db-extra-fields (append db-extra-fields l) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-decode-time t)
  (cond ((== t :now)
         "strftime('%s','now')")
        ((integer? t)
         (number->string t))
        ((string? t)
         (string-append "strftime('%s'," (sql-quote t) ")"))
        ((and (func? t :relative 1) (string? (cadr t)))
         (string-append "strftime('%s','now'," (sql-quote (cadr t)) ")"))
        ((and (func? t :sql 1) (string? (cadr t)))
         (cadr t))
        (else (texmacs-error "sql-time" "invalid time"))))

(define (db-time-constraint)
  (string-append "created <= (" db-time ") AND "
                 "expires >  (" db-time ")"))

(define (db-time-constraint-on x)
  (string-append x ".created <= (" db-time ") AND "
                 x ".expires >  (" db-time ")"))

(define (db-check-now)
  (when (!= db-time "strftime('%s','now')")
    (texmacs-error "db-check-now" "cannot rewrite history")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic private interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-insert-value id attr val)
  (db-check-now)
  (db-sql "INSERT INTO props VALUES (" (sql-quote id)
          ", " (sql-quote attr)
          ", " (sql-quote val)
          ", strftime('%s','now')"
          ", 10675199166)"))

(define (db-insert-time-stamp id)
  (db-check-now)
  (db-sql "INSERT INTO props VALUES (" (sql-quote id)
          ", 'date'"
          ", strftime('%s','now')"
          ", strftime('%s','now')"
          ", 10675199166)"))

(define (db-remove-value id attr val)
  (db-check-now)
  (db-sql "UPDATE props SET expires=strftime('%s','now')"
          " WHERE id=" (sql-quote id)
          " AND attr=" (sql-quote attr)
          " AND val=" (sql-quote val)
          " AND " (db-time-constraint)))

(define (db-remove-field id attr)
  (db-check-now)
  (db-sql "UPDATE props SET expires=strftime('%s','now')"
          " WHERE id=" (sql-quote id)
          " AND attr=" (sql-quote attr)
          " AND " (db-time-constraint)))

(define (db-remove-entry id)
  (db-check-now)
  (db-sql "UPDATE props SET expires=strftime('%s','now')"
          " WHERE id=" (sql-quote id)
          " AND " (db-time-constraint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-field id attr)
  (db-sql* "SELECT DISTINCT val FROM props WHERE id=" (sql-quote id)
           " AND attr=" (sql-quote attr)
           " AND " (db-time-constraint)))

(tm-define (db-get-field-first id attr default)
  (with l (db-get-field id attr)
    (if (null? l) default (car l))))

(tm-define (db-set-field id attr vals)
  (with old-vals (db-get-field id attr)
    (when (!= vals old-vals)
      (db-remove-field id attr)
      (for-each (cut db-insert-value id attr <>) vals))))

(tm-define (db-get-attributes id)
  (db-sql* "SELECT DISTINCT attr FROM props WHERE id=" (sql-quote id)
           " AND " (db-time-constraint)))

(tm-define (db-get-entry id)
  (with r (db-sql "SELECT DISTINCT attr, val FROM props"
                  " WHERE id=" (sql-quote id)
                  " AND " (db-time-constraint))
    (if (nnull? r) (set! r (cdr r)))
    (set! r (reverse r)) ;; NOTE: sqlite3 delivers results in reverse ordering
    (let* ((t (make-ahash-table))
           (attrs (list-remove-duplicates (map car r))))
      (for (line r)
        (with (attr val) line
          (with old (or (ahash-ref t attr) (list))
            (ahash-set! t attr (cons val old)))))
      (map (lambda (attr) (cons attr (reverse (ahash-ref t attr)))) attrs))))

(define (assoc-add l1 l2)
  (append l1 (list-filter l2 (lambda (x) (not (assoc-ref l1 (car x)))))))

(tm-define (db-set-entry id l*)
  (let* ((l (assoc-add l* db-extra-fields))
	 (old-attrs (db-get-attributes id))
         (new-attrs (map car l))
         (rem-attrs (list-difference old-attrs new-attrs)))
    (for (attr rem-attrs)
      (db-remove-field id attr))
    (for (attr new-attrs)
      (db-set-field id attr (assoc-ref l attr)))
    (when (and db-time-stamp? (nin? "date" new-attrs))
      (db-remove-field id "date")
      (db-insert-time-stamp id))))

(tm-define (db-create-entry l)
  (with id (create-unique-id)
    (while (nnull? (db-get-attributes id))
      (set! id (create-unique-id)))
    (db-set-entry id l)
    id))

(tm-define (db-create name type uid)
  (with id (create-unique-id)
    (if (nnull? (db-get-field id "type"))
        (db-create name type uid)
        (begin
          (db-insert-value id "name" name)
          (db-insert-value id "type" type)
          (db-insert-value id "owner" uid)
          id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching database entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-search-join l i)
  (with s (string-append "props AS p" (number->string i))
    (if (null? (cdr l)) s
        (string-append s " JOIN " (db-search-join (cdr l) (+ i 1))))))

(define (db-search-value pi vals)
  (cond ((null? vals)
         (texmacs-error "db-search-value" "invalid search value"))
        ((null? (cdr vals))
         (string-append pi ".val=" (sql-quote (car vals))))
        (else
          (let* ((qvals (map sql-quote vals))
                 (iqvals (list-intersperse qvals ", "))
                 (a (apply string-append iqvals)))
            (string-append pi ".val IN (" a ")")))))

(define (db-search-on l i)
  (with (attr . vals) (car l)
    (let* ((pi (string-append "p" (number->string i)))
           (sid (string-append pi ".id=p1.id"))
           (sattr (string-append pi ".attr=" (sql-quote attr)))
           (spair (string-append sattr " AND " (db-search-value pi vals)))
           (sall (string-append spair " AND " (db-time-constraint-on pi)))
           (q (if (= i 1) sall (string-append sid " AND " sall))))
      (if (null? (cdr l)) q
          (string-append q " AND " (db-search-on (cdr l) (+ i 1)))))))

(tm-define (db-search l)
  (if (null? l)
      (db-sql* "SELECT DISTINCT id FROM props"
               " WHERE " (db-time-constraint))
      (let* ((join (db-search-join l 1))
             (on (db-search-on l 1))
             (sep (if (null? (cdr l)) " WHERE " " ON ")))
        (db-sql* "SELECT DISTINCT p1.id FROM " join sep on))))

(tm-define (db-search-name name)
  (db-search (list (list "name" name))))

(tm-define (db-search-owner owner)
  (db-search (list (list "owner" owner))))
