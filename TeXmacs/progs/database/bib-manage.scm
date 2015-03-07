
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-manage.scm
;; DESCRIPTION : global bibliography management
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-manage)
  (:use (database bib-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing and caching existing BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bib-dir "$TEXMACS_HOME_PATH/system/database")
(define bib-master (url->url (string-append bib-dir "/bib-master.tmdb")))

(define (bib-cache-id f)
  (with-database bib-master
    (let* ((s (url->system f))
           (l (db-search (list (list "source" s)))))
      (and (== (length l) 1) (car l)))))

(define (bib-cache-stamp f)
  (and-with id (bib-cache-id f)
    (with-database bib-master
      (db-get-first id "stamp" #f))))

(define (bib-cache-db f)
  (and-with id (bib-cache-id f)
    (with-database bib-master
      (system->url (db-get-first id "target" #f)))))

(define (bib-cache-up-to-date? f)
  (and-with stamp (bib-cache-stamp f)
    (and (url-exists? f)
         (== (number->string (url-last-modified f)) stamp))))

(define (bib-cache-remove f)
  (and-with id (bib-cache-id f)
    (and-with db (bib-cache-db f)
      (system-remove db)
      (with-database bib-master
        (db-reset-all id)))))

(define (bib-cache-create f)
  (let* ((bib-doc (string-load f))
         (tm-doc (convert bib-doc "bibtex-document" "texmacs-stree"))
         (body (tmfile-extract tm-doc 'body))
         (id (create-unique-id))
         (db (url->url (string-append bib-dir "/" id ".tmdb"))))
    (when body
      (with-database db
        (bib-export body))
      (when (url-exists? db)
        (with-database bib-master
          (db-insert id "source" (url->system f))
          (db-insert id "target" (url->system db))
          (db-insert id "stamp" (number->string (url-last-modified f))))))))

(tm-define (bib-cache-bibtex f)
  (when (not (bib-cache-up-to-date? f))
    (bib-cache-remove f))
  (when (not (bib-cache-id f))
    (bib-cache-create f))
  (when (not (bib-cache-id f))
    (texmacs-error "failed to create bibliographic database"
                   "bib-import-bibtex"))
  (and-with id (bib-cache-id f)
    (url->url (string-append bib-dir "/" id ".tmdb"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default bibliographic database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-bib-db)
  (url->system (string->url "$TEXMACS_HOME_PATH/database/bib.tmdb")))

(define-preferences
  ("bib-db" (default-bib-db) noop))

(tm-define (get-bib-db)
  (get-preference "bib-db"))

(tm-define (set-bib-db val)
  (when (string? val)
    (set-preference "bib-db" val)
    (refresh-now "bib-db-preference")))

(tm-define (get-bib-db-short)
  (with full (system->url (get-bib-db))
    (url->system (url-tail full))))

(tm-define (set-bib-db-short val)
  (when (string? val)
    (with full (system->url (get-bib-db))
      (set-bib-db (url->system (url-relative full (system->url val)))))))

(tm-define (url-bib-db)
  (system->url (get-bib-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bib-retrieve-one name)
  (and-with l (db-search (list (list "name" name)))
    (and (nnull? l)
         (with e (db-import-entry (car l))
           (cons name e)))))

(define (bib-retrieve-several names)
  (if (null? names) (list)
      (let* ((head (bib-retrieve-one (car names)))
             (tail (bib-retrieve-several (cdr names))))
        (if head (cons head tail) tail))))

(define (bib-retrieve-entries-from-one names db)
  (with-database db
    (bib-retrieve-several names)))

(define (bib-retrieve-entries-from names dbs)
  (if (null? dbs) (list)
      (let* ((r (bib-retrieve-entries-from-one names (car dbs)))
             (done (map car r))
             (remaining (list-difference names done)))
        (append r (bib-retrieve-entries-from remaining (cdr dbs))))))

(tm-define (bib-retrieve-entries names . bib-files)
  (set! names (list-remove-duplicates names))
  (let* ((bl (list-filter (map bib-cache-bibtex bib-files) (lambda (ok?) ok?)))
         (al (rcons bl (url-bib-db))))
    (bib-retrieve-entries-from names al)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running bibtex or its internal replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib-generate style doc)
  (with m `(bibtex ,(string->symbol style))
    (module-provide m)
    (bibstyle style doc)))

(tm-define (bib-compile style names . bib-files)
  (let* ((l (apply bib-retrieve-entries (cons names bib-files)))
         (bl (map db->bib (map cdr l)))
         (doc `(document ,@bl)))
    (bib-generate style doc)))
