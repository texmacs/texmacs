
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-db.scm
;; DESCRIPTION : bibliographic database format
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-db)
  (:use (database db-convert)
        (database db-edit)
        (convert bibtex bibtexout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formats of bibliographic resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-format
  ("article"
   (and "author" "title" "journal" "year"
        (optional "key")
        (optional "volume") (optional "number")
        (optional "pages")
        (optional "month")
        (optional "note") (optional "annote")))
  ("book"
   (and (or "author" "editor") "title" "publisher" "year"
        (optional "key")
        (optional "volume") (optional "number")
        (optional "series")
        (optional "address")
        (optional "edition")
        (optional "month")
        (optional "note") (optional "annote")))
  ("booklet"
   (and "title"
        (optional "key")
        (optional "author")
        (optional "howpublished")
        (optional "address")
        (optional "month")
        (optional "year")
        (optional "note") (optional "annote")))
  ("inbook"
   (and (or "author" "editor") "title" "chapter" "publisher" "year"
        (optional "key")
        (optional "volume") (optional "number")
        (optional "series")
        (optional "type")
        (optional "address")
        (optional "edition")
        (optional "month")
        (optional "pages")
        (optional "note") (optional "annote")))
  ("incollection"
   (and "author" "title" "booktitle"
        (optional "crossref")
        (optional "key")
        (optional "pages")
        (optional "publisher")
        (optional "year")
        (optional "editor")
        (optional "volume") (optional "number")
        (optional "series")
        (optional "type")
        (optional "chapter")
        (optional "address")
        (optional "edition")
        (optional "month")
        (optional "note") (optional "annote")))
  ("inproceedings"
   (and "author" "title"
        (optional "crossref")
        (optional "key")
        (optional "booktitle")
        (optional "pages")
        (optional "year")
        (optional "editor")
        (optional "volume") (optional "number")
        (optional "series")
        (optional "address")
        (optional "month")
        (optional "organization")
        (optional "publisher")
        (optional "note") (optional "annote")))
  ;;("conference"
  ;; (and))
  ("manual"
   (and "title"
        (optional "key")
        (optional "author")
        (optional "organization")
        (optional "address")
        (optional "edition")
        (optional "month")
        (optional "year")
        (optional "note") (optional "annote")))
  ("masterthesis"
   (and "author" "title" "school" "year"
        (optional "key")
        (optional "type")
        (optional "address")
        (optional "month")
        (optional "note") (optional "annote")))
  ("misc"
   (and (optional "key")
        (optional "author")
        (optional "title")
        (optional "howpublished")
        (optional "month")
        (optional "year")
        (optional "note") (optional "annote")))
  ("phdthesis"
   (and "author" "title" "school" "year"
        (optional "key")
        (optional "type")
        (optional "address")
        (optional "month")
        (optional "note") (optional "annote")))
  ("proceedings"
   (and "title" "year"
        (optional "key")
        (optional "booktitle")
        (optional "editor")
        (optional "volume") (optional "number")        
        (optional "series")
        (optional "address")
        (optional "month")
        (optional "organization")
        (optional "publisher")
        (optional "note") (optional "annote")))
  ("techreport"
   (and "author" "title" "institution" "year"
        (optional "key")
        (optional "type")
        (optional "number")
        (optional "address")
        (optional "month")
        (optional "note") (optional "annote")))
  ("unpublished"
   (and "author" "title" "note"
        (optional "key")
        (optional "month")
        (optional "year")
        (optional "annote"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion of native BibTeX documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib->db t)
  (cond ((and (tm-func? t 'bib-entry 3)
              (tm-func? (tm-ref t 2) 'document))
         (with l (map bib->db (tm-children (tm-ref t 2)))
           `(db-resource ,(create-unique-id) ,(tm-ref t 0) ,(tm-ref t 1)
                         (document ,@l))))
        ((tm-func? t 'bib-field 2)
         `(db-entry ,(tm-ref t 0) ,(serialize-bibtex-arg (tm-ref t 1))))
        (else t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliography hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define bib-types-list
  (list "article" "book" "booklet" "inbook" "incollection"
        "inproceedings" "conference" "manual" "mastersthesis" "misc"
        "phdthesis" "proceedings" "techreport" "unpublished"))

(tm-define (db-import-post t)
  (set! t (former t))
  (if (not (tm-equal? (db-resource-ref t "type") "bib-entry")) t
      (with x (db-resource-ref t "bib-type")
        (if (not x) t
            (with type (locase-all (tm->string x))
              (if (nin? type bib-types-list) t
                  (db-resource-set (db-resource-remove t "bib-type")
                                   "type" type)))))))

(tm-define (db-export-pre t)
  (set! t (former t))
  (if (and (tm-func? t 'bib-entry 3)
           (tm-func? (tm-ref t 2) 'document))
      (db-export-pre (bib->db t))
      (with x (db-resource-ref t "type")
        (if x (set! x (tm->string x)))
        (if (not (in? x bib-types-list)) t
            (db-resource-set (db-resource-set t "bib-type" x)
                             "type" "bib-type")))))
