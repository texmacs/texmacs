
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

(tm-define (global-database)
  (url-concretize "$TEXMACS_HOME_PATH/server/global.tmdb"))

(tm-define current-database (url-none))

(tm-define-macro (with-database db . body)
  `(with-global current-database ,db
     ,@body))

(tm-define-macro (with-database* db . body)
  `(with-global current-database ,db
     (tmdb-keep-history current-database #f)
     ,@body))

(tm-define (db-reset)
  (set! current-database (url-none)))

(tm-define (db-get-db)
  (if (url-none? current-database)
      (texmacs-error "db-get-db" "no database specified")
      current-database))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define db-time :now)
(tm-define db-time-stamp? #f)
(tm-define db-extra-fields (list))
(tm-define db-limit #f)

(tm-define-macro (with-time t . body)
  `(with-global db-time ,t ,@body))

(tm-define-macro (with-time-stamp on? . body)
  `(with-global db-time-stamp? ,on? ,@body))

(tm-define-macro (with-extra-fields l . body)
  `(with-global db-extra-fields (append db-extra-fields ,l) ,@body))

(tm-define-macro (with-limit limit . body)
  `(with-global db-limit ,limit ,@body))

(tm-define (db-reset)
  (former)
  (set! db-time :now)
  (set! db-time-stamp? #f)
  (set! db-extra-fields (list))
  (set! db-limit #f))

(define (db-get-time*)
  (cond ((== db-time :now) (current-time))
        ((== db-time :always) 0)
        ((number? db-time) db-time)
        ((string? db-time) (string->number db-time))
        (else
          (display* "db-time= " db-time "\n")
          (texmacs-error "db-get-time" "invalid time"))))

(tm-define (db-get-time)
  (exact->inexact (db-get-time*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-set-field id attr vals)
  (tmdb-set-field (db-get-db) id attr vals (db-get-time)))

(tm-define (db-get-field id attr)
  (tmdb-get-field (db-get-db) id attr (db-get-time)))

(tm-define (db-get-field-first id attr default)
  (with l (db-get-field id attr)
    (if (null? l) default (car l))))

(tm-define (db-remove-field id attr)
  (tmdb-remove-field (db-get-db) id attr (db-get-time)))

(tm-define (db-get-attributes id)
  (tmdb-get-attributes (db-get-db) id (db-get-time)))

(tm-define (db-entry-exists? id)
  (nnull? (db-get-field id "name")))

(tm-define (db-get-entry id)
  (tmdb-get-entry (db-get-db) id (db-get-time)))

(tm-define (assoc-add l1 l2)
  (append l1 (list-filter l2 (lambda (x) (not (assoc-ref l1 (car x)))))))

(tm-define (db-set-entry id l)
  ;;(display* "*** set " id " := " l ", " (db-get-db) "\n")
  (let* ((old-l (db-get-entry id))
         (new-l (assoc-add db-extra-fields l)))
    (when (and db-time-stamp? (not (assoc-ref new-l "date")))
      (set! new-l (rcons new-l (list "date" (number->string (current-time))))))
    (tmdb-set-entry (db-get-db) id new-l (db-get-time))))

(tm-define (db-remove-entry id)
  ;;(display* "*** remove " id ", " (db-get-db) "\n")
  (tmdb-remove-entry (db-get-db) id (db-get-time)))

(tm-define (db-create-id)
  (if (url-none? current-database)
      (create-unique-id)
      (with id (create-unique-id)
        (while (nnull? (db-get-attributes id))
          (set! id (create-unique-id)))
        id)))

(tm-define (db-create-entry l)
  (with id (db-create-id)
    (db-set-entry id l)
    id))

(define (rewrite-query q)
  (cond ((func? q :order 2)
         (list 'order (cadr q) (nin? (caddr q) (list #f "#f"))))
        ((func? q :modified) (cons 'modified (cdr q)))
        ((func? q :match) (cons 'contains (cdr q)))
        ((func? q :prefix) (cons 'completes (cdr q)))
        ((func? q :contains) (cons 'contains (cdr q)))
        ((func? q :completes) (cons 'completes (cdr q)))
        (else q)))

(tm-define (db-search l*)
  (with l (map rewrite-query l*)
    (tmdb-query (db-get-db) l (db-get-time) (or db-limit 1000000))))

(tm-define (db-search-name name)
  (db-search (list (list "name" name))))

(tm-define (db-search-owner owner)
  (db-search (list (list "owner" owner))))

(tm-define (db-inspect-history name)
  (tmdb-inspect-history (db-get-db) name))

(tm-define (index-get-completions prefix)
  (tmdb-get-completions (db-get-db) prefix))

(tm-define (index-get-name-completions prefix)
  (tmdb-get-name-completions (db-get-db) prefix))

(tm-define (prefix->queries q)
  (list (list :completes q)))
