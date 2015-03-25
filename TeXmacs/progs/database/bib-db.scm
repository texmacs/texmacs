
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
;; Formats of bibliographic entries
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

(smart-table db-encoding-table
  ((* "article" *) :texmacs)
  ((* "book" *) :texmacs)
  ((* "booklet" *) :texmacs)
  ((* "inbook" *) :texmacs)
  ((* "incollection" *) :texmacs)
  ((* "inproceedings" *) :texmacs)
  ((* "manual" *) :texmacs)
  ((* "masterthesis" *) :texmacs)
  ((* "misc" *) :texmacs)
  ((* "phdthesis" *) :texmacs)
  ((* "proceedings" *) :texmacs)
  ((* "techreport" *) :texmacs)
  ((* "unpublished" *) :texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to native BibTeX documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-bib-keep-case s f?)
  (if (or (== s (locase-all s))
          (and f? (== s (upcase-first (locase-all s)))))
      s `(keepcase ,s)))

(define (db-bib-keep-case* s)
  (db-bib-keep-case s #f))

(define (db-bib-protect t f?)
  (cond ((tm-atomic? t)
         (let* ((l (string-decompose t " "))
                (fl (list-filter l (lambda (s) (!= s ""))))
                (nn? (nnull? fl))
                (tail (map db-bib-keep-case* (if nn? (cdr fl) fl)))
                (r (if f? (cons (db-bib-keep-case (car fl) f?) tail) tail)))
           (apply tmconcat (list-intersperse r " "))))
        (else
          (with l (tm-children t)
            (if (null? l) t
                (cons* (tm-label t)
                       (db-bib-protect (car l) f?)
                       (map (cut db-bib-protect <> #f) (cdr l))))))))

(define (db-bib-name t)
  (let* ((c (if (tm-func? t 'concat) (tm-children t) (list t)))
         (fi (list-find-index c (lambda (x)
                                  (or (tm-func? x 'name 1)
                                      (tm-func? x 'name-von 1)
                                      (tm-func? x 'name-jr 1)))))
         (f (if fi (sublist c 0 fi) c))
         (l (list-find c (lambda (x) (tm-func? x 'name 1))))
         (v (list-find c (lambda (x) (tm-func? x 'name-von 1))))
         (j (list-find c (lambda (x) (tm-func? x 'name-jr 1)))))
    (while (and (nnull? f) (string? (cAr f)) (string-ends? (cAr f) " "))
      (set! f (rcons (cDr f) (string-drop-right (cAr f) 1))))
    `(bib-name ,(apply tmconcat f)
               ,(if v (tm-ref v 0) "")
               ,(if l (tm-ref l 0) "")
               ,(if j (tm-ref j 0) ""))))

(define (db-bib-names t)
  (if (not (tm-func? t 'concat))
      `(bib-names ,(db-bib-name t))
      (let* ((l (tm-children t))
             (ls (list-scatter l (cut == <> '(name-sep)) #f))
             (c (map (lambda (x) (apply tmconcat x)) ls))
             (r (map db-bib-name c)))
        (if (null? r) (set! r (list `(bib-name "" "" "" ""))))
        `(bib-names ,@r))))

(define (db-bib-pages t)
  (if (not (tm-atomic? t)) t
      (let* ((l (string-decompose t "-"))
             (fl (list-filter l (lambda (s) (!= s "")))))
        (if (<= (length l) 1) t `(bib-pages ,@fl)))))

(define (db-bib-sub-sub type var val)
  (cond ((and (== var "title")
              (in? type '("article" "booklet" "incollection" "inproceedings"
                          "masterthesis" "misc" "phd-thesis" "techreport"
                          "unpublished")))
         (db-bib-protect val #t))
        ((or (== var "type") (== var "mtype"))
         (db-bib-protect val #f))
        ((or (== var "author") (== var "editor"))
         (db-bib-names val))
        ((== var "pages")
         (db-bib-pages val))
        (else val)))

(define (db-bib-sub t type)
  (cond ((tm-func? t 'db-field 2)
         (let* ((var (tm-ref t 0))
                (val (tm-ref t 1)))
           `(bib-field ,var ,(db-bib-sub-sub type var val))))
        (else t)))

(tm-define (db->bib t)
  (set! t (tm->stree t))
  (cond ((and (tm-func? t 'db-entry 5)
              (tm-atomic? (tm-ref t 1))
              (in? (tm-ref t 1) bib-types-list)
              (tm-func? (tm-ref t :last) 'document))
         (let* ((type (tm-ref t 1))
                (name (tm-ref t 2))
                (l (map (cut db-bib-sub <> type)
                        (tm-children (tm-ref t :last)))))
           `(bib-entry ,type ,name (document ,@l))))
        (else t)))

(tm-define (bibify-buffer)
  (display* "Determining bib\n\n")
  (with t (buffer-tree)
    (when (tm-func? t 'document)
      (for-each (lambda (x)
                  (display* "Transforming " (tm->stree x) "\n\n")
                  (display* "Yields " (db->bib x) "\n\n"))
                (tm-children t)))))

(tm-define (as-bib)
  (with t (buffer-tree)
    (when (tm-func? t 'document)
      (with r `(document ,@(map db->bib (tm-children t)))
        (tree-assign! t r)
        (set-style-list `("bibliography"))))))

(tm-define (buffer-as-bibtex)
  (with t (buffer-tree)
    (when (tm-func? t 'document)
      (with r `(document ,@(map db->bib (tm-children t)))
        (display* (convert r "texmacs-stree" "bibtex-document") "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion from native BibTeX documents and hook when saving databases
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
           `(db-field ,var ,(bib-db-unmacro (bib-db-sub-sub type var val)))))
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
           `(db-entry ,id ,type* ,name (document) (document ,@l))))
        (else t)))

(tm-define (db-save-pre t)
  (former (bib->db t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load and save bibliographies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define bib-types-list
  (smart-ref db-kind-table "bib"))

(tm-define (bib-load)
  (db-load-types bib-types-list))

(tm-define (bib-save t)
  (db-save-types t bib-types-list))
