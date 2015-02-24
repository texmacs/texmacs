
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
