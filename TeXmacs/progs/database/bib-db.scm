
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

(smart-table db-kind-table
  ("bib" ("article" "book" "booklet" "inbook" "incollection"
          "inproceedings" "conference" "manual" "mastersthesis" "misc"
          "phdthesis" "proceedings" "techreport" "unpublished")))

(smart-table db-format-table
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
;; Conversion of native BibTeX documents and hook when exporting databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bib-db-unmacro t)
  (cond ((tm-atomic? t) (tm->string t))
        ((tm-func? t 'keepcase 1)
         (bib-db-unmacro (tm-ref t 0)))
        ((tm-func? t 'bib-names)
         (let* ((l (map bib-db-unmacro (tm-children t)))
                (r (list-intersperse l '(name-sep))))
           (apply tmconcat r)))
        ((tm-func? t 'bib-name 4)
         (with (f v l j) (tm-children (tm->stree t))
           (with r (append (if (== f "") (list) (list f))
                           (if (== v "") (list) (list " " `(name-von ,v)))
                           (if (== l "") (list) (list " " `(name ,l)))
                           (if (== j "") (list) (list " " `(name-jr ,j))))
             (while (and (nnull? r) (== (car r) " "))
               (set! r (cdr r)))
             (apply tmconcat (map bib-db-unmacro r)))))
        ((tm-func? t 'bib-pages)
         (let* ((l (map bib-db-unmacro (tm-children t)))
                (r (list-intersperse l "--")))
           (apply tmconcat r)))
        ((tm-func? t 'concat)
         (with l (map bib-db-unmacro (tm-children t))
           (apply tmconcat l)))
        (else
          (cons (tm-label t) (map bib-db-unmacro (tm-children t))))))

(define (bib-db-locase t first?)
  (cond ((tm-atomic? t)
         (with r (locase-all (tm->string t))
           (if first? (upcase-first r) r)))
        ((tm-func? t 'keepcase 1) t)
        (else
          (with l (tm-children t)
            (if (null? l) t
                (cons* (tm-label t)
                       (bib-db-locase (car l) first?)
                       (map (cut bib-db-locase <> #f) (cdr l))))))))

(define (bib-db-sub-sub type var val)
  (cond ((and (== var "title")
              (in? type '("article" "booklet" "incollection" "inproceedings"
                          "masterthesis" "misc" "phd-thesis" "techreport"
                          "unpublished")))
         (bib-db-locase val #t))
        ((or (== var "type") (== var "mtype"))
         (bib-db-locase val #f))
        (else val)))

(define (bib-db-sub t type)
  (cond ((tm-func? t 'bib-field 2)
         (let* ((var (tm-ref t 0))
                (val (tm-ref t 1)))
           `(db-entry ,var ,(bib-db-unmacro (bib-db-sub-sub type var val)))))
        (else t)))

(tm-define (bib->db t)
  (cond ((and (tm-func? t 'bib-entry 3)
              (tm-func? (tm-ref t 2) 'document))
         (let* ((id (create-unique-id))
                (type (tm->string (tm-ref t 0)))
                (type* (if (== type "conference") "inproceedings" type))
                (name (tm-ref t 1))
                (l (map (cut bib-db-sub <> type*)
                        (tm-children (tm-ref t 2)))))
           `(db-resource ,id ,type* ,name (document ,@l))))
        (else t)))

(tm-define (db-export-pre t)
  (former (bib->db t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import and export bibliographies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define bib-types-list
  (smart-ref db-kind-table "bib"))

(tm-define (bib-import)
  (db-import-types bib-types-list))

(tm-define (bib-export t)
  (db-export-types t bib-types-list))
