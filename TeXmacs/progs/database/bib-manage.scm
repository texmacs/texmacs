
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

(define bib-master
  (string->url "$TEXMACS_HOME_PATH/system/bib-cache/bib-master.tmdb"))

(define (bib-cache-rid f)
  (with-database bib-master
    (let* ((s (url->system f))
           (l (db-search (list (list "source" s)))))
      (and (== (length l) 1) (car l)))))

(define (bib-cache-stamp f)
  (and-with rid (bib-cache-rid f)
    (with-database bib-master
      (db-get rid "stamp"))))

(define (bib-cache-db f)
  (and-with rid (bib-cache-rid f)
    (with-database bib-master
      (db-get rid "target"))))

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

(tm-define (bib-import-bibtex f)
  (let* ((bib-doc (string-load f))
         (tm-doc (convert bib-doc "bibtex-document" "texmacs-document"))
         (name (create-unique-id))
         (fname (string-append "$TEXMACS_HOME_PATH/system/bib-cache/" name)))
    (display* tm-doc "\n")))
