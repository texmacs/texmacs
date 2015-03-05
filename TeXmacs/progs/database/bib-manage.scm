
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
;; Importing BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define bib-dir "$TEXMACS_HOME_PATH/system/bib-cache")
;;(define bib-master (string-append bib-dir "/bib-master.tmdb"))
(define bib-dir "~/bibtest")
(define bib-master (url->url (string-append bib-dir "/bib-master.tmdb")))

(define (bib-cache-rid f)
  (with-database bib-master
    (let* ((s (url->system f))
           (l (db-search (list (list "source" s)))))
      (and (== (length l) 1) (car l)))))

(define (bib-cache-stamp f)
  (and-with rid (bib-cache-rid f)
    (with-database bib-master
      (db-get-first rid "stamp" #f))))

(define (bib-cache-db f)
  (and-with rid (bib-cache-rid f)
    (with-database bib-master
      (system->url (db-get-first rid "target" #f)))))

(define (bib-cache-up-to-date? f)
  (and-with stamp (bib-cache-stamp f)
    (and (url-exists? f)
         (== (number->string (url-last-modified f)) stamp))))

(define (bib-cache-remove f)
  (and-with rid (bib-cache-rid f)
    (and-with db (bib-cache-db f)
      (system-remove db)
      (with-database bib-master
        (db-reset-all rid)))))

(define (bib-cache-create f)
  (let* ((bib-doc (string-load f))
         (tm-doc (convert bib-doc "bibtex-document" "texmacs-stree"))
         (body (tmfile-extract tm-doc 'body))
         (rid (create-unique-id))
         (db (string-append bib-dir "/" rid ".tmdb")))
    (when body
      (with-database db
        (bib-export body))
      (with-database bib-master
        (db-insert rid "source" (url->system f))
        (db-insert rid "target" (url->system db))
        (db-insert rid "stamp" (number->string (url-last-modified f)))))))

(tm-define (bib-import-bibtex f)
  (when (not (bib-cache-up-to-date? f))
    (bib-cache-remove f))
  (when (not (bib-cache-rid f))
    (bib-cache-create f))
  (when (not (bib-cache-rid f))
    (texmacs-error "failed to create bibliographic database"
                   "bib-import-bibtex"))
  (bib-cache-rid f))
