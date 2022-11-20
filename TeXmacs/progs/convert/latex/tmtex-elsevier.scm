
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-elsevier.scm
;; DESCRIPTION : special conversions for elsevier styles
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-elsevier)
  (:use (convert latex tmtex)))

(tm-define (tmtex-transform-style x)
  (:mode elsevier-style?)
  (cond ((== x "elsart") "elsart")
        ((== x "elsarticle") "elsarticle")
        ((== x "ifac") "ifacconf")
        ((== x "jsc") `("amsthm" "elsart"))
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of elsevier style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define note-counter 0)
(define author-counter 0)
(define clustered? #f)

(define (init-elsevier body)
  (set! clustered? #f)
  (set! note-counter 0)
  (set! author-counter 0))

(tm-define (tmtex-style-init body)
  (:mode elsevier-style?)
  (init-elsevier body))

(tm-define (tmtex-style-init body)
  (:mode ifac-style?)
  (init-elsevier body)
  (set! tmtex-packages (cons "cite-author-year" tmtex-packages))
  (latex-set-packages '("natbib"))
  )

(tm-define (tmtex-style-init body)
  (:mode jsc-style?)
  (init-elsevier body)
  ;;(set! tmtex-packages (cons "cite-author-year" tmtex-packages))
  (latex-set-packages '("amsthm" "yjsco" ;;"natbib"
                        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack for ifac incompatibility with hyperref package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-as-use-package l)
  (:require (latex-ifacconf-style?))
  (if (nin? "hyperref" l)
      (former l)
      (let* ((l* (list-remove l "hyperref"))
             (s1 (if (null? l*) "" (former l*)))
             (s2 (string-append
                  "\\makeatletter\n"
                  "\\let\\old@ssect\\@ssect\n"
                  "\\makeatother\n"
                  "\\usepackage{hyperref}\n"
                  "\\makeatletter\n"
                  "\\def\\@ssect#1#2#3#4#5#6{%\n"
                  "  \\NR@gettitle{#6}%\n"
                  "  \\old@ssect{#1}{#2}{#3}{#4}{#5}{#6}%\n"
                  "}\n"
                  "\\makeatother\n")))
        (string-append s1 s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack for incomplete ifac list environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-extra-preamble)
  (:require (latex-ifacconf-style?))
  (string-append "\\newcommand{\\labelitemiii}{\\labelitemi}\n"
                 "\\newcommand{\\labelitemiv}{\\labelitemii}\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preprocessing data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-style-preprocess doc)
  (:mode elsevier-style?)
  (elsevier-create-frontmatter doc))

(define (elsarticle-frontmatter? t)
  (or (func? t 'abstract-data) (func? t 'doc-data) (func? t 'abstract)))

(define (partition l pred?)
  (if (npair? l) l
    (letrec ((npred? (lambda (x) (not (pred? x)))))
      (if (pred? (car l))
        (receive (h t) (list-break l npred?)
          (cons h (partition t pred?)))
        (receive (h t) (list-break l pred?)
          (cons h (partition t pred?)))))))

(define (elsevier-create-frontmatter t)
  (if (or (npair? t) (npair? (cdr t))) t
    (with l (map elsarticle-frontmatter? (cdr t))
      (if (in? #t l)
        (with parts (partition (cdr t) elsarticle-frontmatter?)
          `(,(car t) ,@(map (lambda (x)
                              (if (elsarticle-frontmatter? (car x))
                                `(elsevier-frontmatter (,(car t) ,@x))
                                `(,(car t) ,@x))) parts)))
        `(,(car t) ,@(map elsevier-create-frontmatter (cdr t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier specific customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-elsevier-frontmatter s l)
  (:mode elsevier-style?)
  `((!begin "frontmatter") ,(tmtex (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsarticle specific title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-replace-documents t)
  (:mode elsevier-style?) t)

(tm-define (springer-note-ref l r)
  (if (list? r)
    (set! r (tex-concat* (list-intersperse r ",")))
    (set! r (string-append l r)))
  `(tnoteref ,r))

(tm-define (tmtex-doc-subtitle-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "sub-" (car l)))

(tm-define (tmtex-doc-subtitle-label s l)
  (:mode elsevier-style?)
  (with label (string-append "sub-" (car l))
    `(tsubtitletext (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-note-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "note-" (car l)))

(tm-define (tmtex-doc-note-label s l)
  (:mode elsevier-style?)
  (with label (string-append "note-" (car l))
    `(tnotetext (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-date-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "date-" (car l)))

(tm-define (tmtex-doc-date-label s l)
  (:mode elsevier-style?)
  (with label (string-append "date-" (car l))
    `(tdatetext (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-misc-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "misc-" (car l)))

(tm-define (tmtex-doc-misc-label s l)
  (:mode elsevier-style?)
  (with label (string-append "misc-" (car l))
    `(tmisctext (!option ,label) ,(tmtex (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier specific authors macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (springer-author-note-ref l r)
  (if (list? r)
    (set! r (tex-concat* (list-intersperse r ",")))
    (set! r (string-append l r)))
  `(fnref ,r))

(tm-define (tmtex-author-note-ref s l)
  (:mode elsevier-style?)
  (springer-author-note-ref "author-note-" (car l)))

(tm-define (tmtex-author-note-label s l)
  (:mode elsevier-style?)
  (with label (string-append "author-note-" (car l))
    `(fntext (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-author-misc-ref s l)
  (:mode elsevier-style?)
  (springer-author-note-ref "author-misc-" (car l)))

(tm-define (tmtex-author-misc-label s l)
  (:mode elsevier-style?)
  (with label (string-append "author-misc-" (car l))
    `(fmtext (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-author-affiliation t)
  (:mode elsevier-style?)
  `(address ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation-ref s l)
  (:mode elsevier-style?)
  (springer-author-note-ref "affiliation-" (car l)))

(tm-define (tmtex-author-affiliation-label s l)
  (:mode elsevier-style?)
  (with label (string-append "affiliation-" (car l))
    `(address (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-author-email t)
  (:mode elsevier-style?)
  `(ead ,(tmtex (cadr t))))

(tm-define (tmtex-author-email-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "author-email-" (car l)))

(tm-define (tmtex-author-email-label s l)
  (:mode elsevier-style?)
  `(ead ,(tmtex (cadr l))))

(tm-define (tmtex-author-homepage t)
  (:mode elsevier-style?)
  `(ead (!option "url") ,(tmtex (cadr t))))

(tm-define (tmtex-author-homepage-ref s l)
  (:mode elsevier-style?)
  (springer-note-ref "author-url-" (car l)))

(tm-define (tmtex-author-homepage-label s l)
  (:mode elsevier-style?)
  `(ead (!option "url") ,(tmtex (cadr l))))

(tm-define (tmtex-author-name t)
  (:mode elsevier-style?)
  `(author ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsart and IFAC specific title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-replace-documents t)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (if (npair? t) t
    (with (r s) (list (car t) (map tmtex-replace-documents (cdr t)))
      (if (!= r 'document) `(,r ,@s)
        `(concat ,@(list-intersperse s '(next-line)))))))

(tm-define (springer-note-ref l r)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (if (list? r)
    `(!concat ,@(map (lambda (x) `(thanksref ,x)) r))
    `(thanksref ,(string-append l r))))

(tm-define (tmtex-doc-subtitle-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "sub-" (car l))
    `(thankssubtitle (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-note-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "note-" (car l))
    `(thanks (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-date-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "date-" (car l))
    `(thanksdate (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-doc-misc-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "misc-" (car l))
    `(thanksmisc (!option ,label) ,(tmtex (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsart specific authors macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (springer-author-note-ref l r)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (springer-note-ref l r))

(tm-define (tmtex-author-note-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "author-note-" (car l))
    `(thanks (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-author-misc-label s l)
  (:mode elsevier-style?)
  (:require (or (elsart-style?) (jsc-style?) (ifac-style?)))
  (with label (string-append "author-misc-" (car l))
    `(thanksamisc (!option ,label) ,(tmtex (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IFAC specific authors macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-author-email-label s l)
  (:mode ifac-style?)
  (with label (string-append "author-email-" (car l))
    `(thanksemail (!option ,label) ,(tmtex (cadr l)))))

(tm-define (tmtex-author-homepage-label s l)
  (:mode ifac-style?)
  (with label (string-append "author-url-" (car l))
    `(thankshomepage (!option ,label) ,(tmtex (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier title and author preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-prepare-doc-data l)
  (:mode elsevier-style?)
  (set! clustered?
    (or
      (contains-stree? l '(doc-title-options "cluster-by-affiliation"))
      (contains-stree? l '(doc-title-options "cluster-all"))))
  (set! l (map tmtex-replace-documents l))
  (set! l (make-references l 'doc-subtitle #f #f))
  (set! l (make-references l 'doc-note #f #f))
  (set! l (make-references l 'doc-misc #f #f))
  (set! l (make-references l 'doc-date #f #f))
  (set! l (make-references l 'author-note #t #f))
  (set! l (make-references l 'author-misc #t #f))
  (if (ifac-style?)
    (begin
      (set! l (make-references l 'author-email #t #f))
      (set! l (make-references l 'author-homepage #t #f))))
  (if clustered?
    (set! l (make-references l 'author-affiliation #t #f)))
  l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier title and author presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                subtitles-l dates-l miscs-l notes-l tr ar)
  (:mode elsevier-style?)
  (let* ((authors (filter nnull? authors))
         (authors (if (null? authors) '()
                    `((!paragraph ,@authors))))
         (titles (tmtex-concat-Sep (map cadr titles)))
         (notes  `(,@subtitles ,@dates ,@miscs ,@notes))
         (notes  (if (null? notes) '()
                   `(,(springer-note-ref "" (map cadr notes)))))
         (result `(,@titles ,@notes))
         (result (if (null? result) '() `((title (!concat ,@result)))))
         (result `(,@result ,@subtitles-l ,@notes-l
                   ,@miscs-l ,@dates-l ,@authors)))
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-make-author names affs emails urls miscs notes
                              affs* emails* urls* miscs* notes*)
  (:mode elsevier-style?)
  (let* ((names  (tmtex-concat-Sep (map cadr names)))
         (notes* (if (ifac-style?)
                   `(,@emails* ,@urls* ,@miscs* ,@notes*)
                   `(,@miscs* ,@notes*)))
         (notes* (if (null? notes*) '()
                   `(,(springer-author-note-ref "" (map cadr notes*)))))
         (affs*  (if (null? affs*) '()
                   `((!option
                       (!concat ,@(list-intersperse (map cadr affs*) ","))))))
         (result `(,@names ,@notes*))
         (result (if (null? result) '()
                   `((author ,@affs* (!concat ,@result)))))
         (result `(,@result ,@affs ,@emails ,@urls ,@miscs ,@notes)))
    (if (null? result) '() `(!paragraph ,@result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier abstract macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-abstract-keywords t)
  (:mode elsevier-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!concat (sep) " "))
    `((!begin "keyword") (!concat ,@args))))

(tm-define (tmtex-abstract-msc t)
  (:mode elsevier-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!concat (sep) " "))
    `(!concat (MSC) " " (!concat ,@args))))

(tm-define (tmtex-abstract-pacs t)
  (:mode elsevier-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!concat (sep) " "))
    `(!concat (PACS) " " (!concat ,@args))))

(tm-define  (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (:mode elsevier-style?)
  (if (or (nnull? msc) (nnull? pacs) (nnull? acm) (nnull? arxiv))
    (set! keywords
      `(((!begin "keyword") (!document ,@(map cadr keywords)
                                       ,@pacs ,@msc ,@acm ,@arxiv)))))
  `(!document ,@abstract ,@keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Elsevier style is quite ugly.
;; Transform equations into eqnarray* for more uniform alignment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-equation s l)
  (:mode elsevier-style?)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex (car l))))
    (tmtex-env-reset "mode")
    (if (== s "equation")
        (list (list '!begin "eqnarray") r)  ;; FIXME: why do elsequation
        (list (list '!begin "eqnarray*") r) ;; and elsequation* not work?
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The elsarticle class does not insert a 'References' section title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(tm-define (tmtex-bib t)
;;  (:mode elsevier-style?)
;;  (:require (elsarticle-style?))
;;  (tmtex-biblio (car t) (cdr t) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier specific macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:mode elsevier-style?)
  (:require (elsarticle-style?))
  (comma #f))

(smart-table latex-texmacs-preamble
  (:mode elsevier-style?)
  (:require (elsarticle-style?))
  (qed (!append (renewcommand "\\qed" "") "\n")))
