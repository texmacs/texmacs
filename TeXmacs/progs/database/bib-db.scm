
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
        (convert bibtex init-bibtex)
        (convert bibtex bibtexout)))

(tm-define (bib-database) (user-database "bib"))

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
  ("conference"
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
  ("mastersthesis"
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
;; Conversion to native BibTeX documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (recursive-locase-all t)
  (if (tm-atomic? t)
      (locase-all t)
      (cons (tm-label t) (map recursive-locase-all (tm-children t)))))

(define (db-bib-keep-case s f?)
  (if (or (== s (locase-all s))
          (and f? (== s (upcase-first (locase-all s)))))
      s `(keepcase ,s)))

(define (db-bib-keep-case* s)
  (db-bib-keep-case s #f))

(define (db-bib-protect-strings l f?)
  (if (null? l) l
      (cons (db-bib-keep-case (car l) f?)
            (db-bib-protect-strings (cdr l) (and f? (== (car l) ""))))))

(define (db-bib-spaces? t)
  (cond ((string? t)
         (list-and (map (cut == <> "") (string-decompose t " "))))
        ((tm-func? t 'concat)
         (list-and (map db-bib-spaces? (tm-children t))))
        ((with-like? t)
         (db-bib-spaces? (cAr t)))
        (else #f)))

(define (db-bib-protect-list l f?)
  (if (null? l) l
      (cons (db-bib-protect (car l) f?)
            (db-bib-protect-list (cdr l) (and f? (db-bib-spaces? (car l)))))))

(define (db-bib-protect t f?)
  (cond ((tm-atomic? t)
         (let* ((l (string-decompose t " "))
                (r (db-bib-protect-strings l f?)))
           (apply tmconcat (list-intersperse r " "))))
        ((tm-func? t 'concat)
         (with l (db-bib-protect-list (tm-children t) f?)
           (apply tmconcat l)))
        ((and (with-like? t) (not (tm-func? t 'math)))
         (rcons (cDr t) (db-bib-protect (cAr t) f?)))
        ((== (recursive-locase-all t) t) t)
        (else `(keepcase ,t))))

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
      (let* ((t* (string-replace t "" "-"))
             (l (string-decompose t* "-"))
             (fl (list-filter l (lambda (s) (!= s "")))))
        (if (<= (length l) 1) `(bib-pages ,t) `(bib-pages ,@fl)))))

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
  (cond ((and (db-entry-any? t)
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

(tm-define db-bib-origin #f)

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
                (r (list-intersperse l "")))
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
        ((or (tm-func? t 'verbatim) (tm-func? t 'slink)) t)
        ((and (tm-func? t 'with 3)
              (tm-equal? (tm-ref t 0) "font-family")
              (tm-equal? (tm-ref t 1) "tt")) t)
        ((and (tm-func? t 'with 3)
              (tm-equal? (tm-ref t 0) "math-font")) t)
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

(define (db-get-origin)
  (if db-bib-origin
      (list `(fb-field "origin" ,db-bib-origin))
      (list)))

(tm-define (bib->db t)
  (cond ((and (tm-func? t 'bib-entry 3)
              (tm-func? (tm-ref t 2) 'document))
         (let* ((id (with-database (bib-database) (db-create-id)))
                (date (number->string (current-time)))
                (type (tm->string (tm-ref t 0)))
                (type* (if (== type "conference") "inproceedings" type))
                (name (tm-ref t 1))
                (h `((db-field "contributor" ,(get-default-user))
                     (db-field "modus" "imported")
                     ,@(db-get-origin)
                     (db-field "date" ,date)))
                (l (map (cut bib-db-sub <> type*)
                        (tm-children (tm-ref t 2)))))
           `(db-entry ,id ,type* ,name (document ,@h) (document ,@l))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib->db* t)
  (cond ((tm-atomic? t) t)
        ((tm-func? t 'bib-entry 3) (bib->db t))
        ((tm-func? t 'style) (tm-replace t "bibliography" "database-bib"))
        (else (cons (tm-label t) (map bib->db* (tm-children t))))))

(tm-define (zealous-bib-import s)
  ;; NOTE: used in conservative_bib.cpp
  (let* ((t (bibtex->texmacs (parse-bibtex-document s)))
         (body (tmfile-extract (bib->db* t) 'body)))
    (tm->tree body)))

(tm-define (tmbib-snippet->texmacs s)
  (with t (bibtex->texmacs (parse-bibtex-snippet s))
    (bib->db* t)))

(define (tmbib-import s att?)
  (let* ((t (bibtex->texmacs (parse-bibtex-document s)))
         (doc (bib->db* t))
         (tm (if (tmfile-extract doc 'TeXmacs) (list)
                 (list `(TeXmacs ,(texmacs-version)))))
         (body (tmfile-extract doc 'body))
         (att `(collection (associate "bibtex-source" ,s)
                           (associate "bibtex-target" ,body))))
    (if att?
        `(,(tm-label doc) ,@tm ,@(tm-children doc) (attachments ,att))
        `(,(tm-label doc) ,@tm ,@(tm-children doc)))))

(tm-define (tmbib-document->texmacs s)
  (tmbib-import s #t))

(tm-define (tmbib-document->texmacs* s)
  (tmbib-import s #f))

(define (db->bib* t)
  (cond ((tm-atomic? t) t)
        ((db-entry-any? t) (db->bib t))
        ((tm-func? t 'style) (tm-replace t "database-bib" "bibliography"))
        (else (cons (tm-label t) (map db->bib* (tm-children t))))))

(tm-define (zealous-bib-export t)
  ;; NOTE: used in conservative_bib.cpp
  (texmacs->tmbib-snippet (tm->stree t)))

(tm-define (texmacs->tmbib-snippet t)
  (with u (db->bib* t)
    (serialize-bibtex (texmacs->bibtex u))))

;; (tm-define (texmacs->tmbib-document doc)
;;   (let* ((u (db->bib* doc))
;;          (s (serialize-bibtex (texmacs->bibtex u)))
;;          (body (tmfile-extract doc 'body))
;;          (att (tmfile-extract doc 'attachments))
;;          (src (collection-ref att "bibtex-source"))
;;          (obj (collection-ref att "bibtex-target")))
;;     (if (and body src obj)
;;         (conservative-bib-export src obj s body)
;;         s)))

(tm-define (texmacs->tmbib-document doc)
  (let* ((body (tmfile-extract doc 'body))
         (att (tmfile-extract doc 'attachments))
         (src (collection-ref att "bibtex-source"))
         (obj (collection-ref att "bibtex-target")))
    (if (and body src obj)
        (conservative-bib-export obj src body)
        (texmacs->tmbib-snippet body))))
