
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization of elsevier style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define note-counter 0)
(define author-counter 0)
(define elsevier-thanks '())
(define elsevier-abstract #f)

(tm-define (init-elsevier body)
  (:synopsis "Initialize Elsevier style")
  (set! note-counter 0)
  (set! author-counter 0)
  (set! elsevier-thanks '())
  (with l (select body '(:* abstract))
    (set! elsevier-abstract (and (nnull? l) (list-2? (car l)) (cadar l)))))

(tm-define (tmtex-style-init body)
  (:mode elsevier-style?)
  (init-elsevier body))

(define (ref-note)
  (number->string note-counter))

(define (refstep-note)
  (set! note-counter (+ note-counter 1))
  (string-append "note-" (ref-note)))

(define (ref-author)
  (number->string author-counter))

(define (refstep-author)
  (set! author-counter (+ author-counter 1))
  (string-append "author-" (ref-author)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsarticle title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-elsarticle-notes)
  (if (== note-counter 0) ""
    (let* ((notes (map number->string (.. 1 (1+ note-counter))))
           (notes (map (cut string-append "note-" <>) notes))
           (notes (apply string-append (list-intersperse notes ","))))
    `(tnoteref ,notes))))

(tm-define (tmtex-elsevier-title t)
  `(title ,(tmtex `(!concat ,(cadr t) ,(list-elsarticle-notes)))))

(tm-define (tmtex-elsevier-note t)
  `(tnotetext (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-date t)
  `(tdatetext (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-subtitle t)
  `(tsubtitletext (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-misc t)
  `(tmisctext (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-auth-note t)
  `(fntext (!option ,(refstep-author)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-auth-note* t)
  (if (string-starts? (third t) "authref-")
    `(,(third t) (fntext (!option ,(third t)) ,(tmtex (fourth t)))) '()))

(define (tmtex-elsevier-affiliation t)
  `(address ,(tmtex (cadr t))))

(define (tmtex-elsevier-affiliation* t)
  `(,(third t) (address (!option ,(third t)) ,(tmtex (fourth t)))))

(define (tmtex-elsevier-email t)
  `(ead ,(tmtex (cadr t))))

(define (tmtex-elsevier-email* t)
  `(,(third t) (ead ,(tmtex (fourth t)))))

(define (tmtex-elsevier-homepage t)
  `(ead (!option "url") ,(tmtex (cadr t))))

(define (tmtex-elsevier-homepage* t)
  `(,(third t) (ead (!option "url") ,(tmtex (fourth t)))))

(tm-define (tmtex-elsevier-name t affref fnref)
  (set! affref (list-intersperse affref ","))
  (if (nnull? fnref)
    (with fnref (tex-concat* (list-intersperse fnref ","))
      (set! t `(!concat ,t (fnref ,fnref)))))
  (if (null? affref)
    `(author ,t)
    `(author (!option ,(tex-concat* affref)) ,t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsart specific title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (thanksref t)
  `(thanksref ,t))

(define (list-elsart-notes)
  (if (== note-counter 0) ""
    (let* ((notes (map number->string (.. 1 (1+ note-counter))))
           (notes (map (lambda (x) (string-append "note-" x)) notes))
           (notes (map thanksref notes)))
    (tex-concat* notes))))

(tm-define (tmtex-elsevier-title t)
  (:mode elsart-style?)
  `(title ,(tmtex `(!concat ,(cadr t) ,(list-elsart-notes)))))

(tm-define (tmtex-elsevier-note t)
  (:mode elsart-style?)
  `(thanks (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-date t)
  (:mode elsart-style?)
  `(thanksdate (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-subtitle t)
  (:mode elsart-style?)
  `(thankssubtitle (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-misc t)
  (:mode elsart-style?)
  `(thanksmisc (!option ,(refstep-note)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-auth-note t)
  (:mode elsart-style?)
  `(thanks (!option ,(refstep-author)) ,(tmtex (cadr t))))

(tm-define (tmtex-elsevier-auth-note* t)
  (:mode elsart-style?)
  (if (string-starts? (third t) "authref-")
    `(,(third t) (thanks (!option ,(third t)) ,(tmtex (fourth t)))) '()))

(tm-define (tmtex-elsevier-name t affref fnref)
  (:mode elsart-style?)
  (set! affref (list-intersperse affref ","))
  (if (nnull? fnref)
    (with fnref (map thanksref fnref)
      (set! t `(!concat ,t ,@fnref))))
  (if (null? affref)
    `(author ,t)
    `(author (!option ,(tex-concat* affref)) ,t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier miscellanous macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-select-args-by-func n l)
  (filter (lambda (x) (func? x n)) l))

(define (elsevier-split-authors t)
  (if (and (pair? t) (pair? (cdr t)) (func? (cadr t) 'concat))
    (with l (filter (lambda (x) (!= ", " x)) (cdadr t))
           (if (null? (filter (lambda (x) (not (func? x 'doc-note-ref))) l))
             l `(,t))) `(,t)))

(define (elsevier-get-names t)
  (cond ((func? t 'doc-note-ref) (elsevier-get-names (cAr t)))
        ((func? t 'author-name) (cadr t))
        (else t)))

(define (elsevier-get-name-refs t)
  (cond ((func? t 'doc-note-ref) (append `(,(cADr t))
                                         (elsevier-get-name-refs (cAr t))))
        ((func? t 'author-name) (elsevier-get-name-refs (cadr t)))
        (else '())))

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

(tm-define (elsevier-create-frontmatter t)
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
;; Elsevier non clustered title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-elsevier-author t)
  (if (or (npair? t) (npair? (cdr t)) (not (func? (cadr t) 'author-data))) '()
    (let* ((datas        (cdadr t))
           ;; notes and miscs needed in first position due to side effects
;           (miscs        (map tmtex-elsevier-auth-misc
;                              (tmtex-select-args-by-func 'author-misc datas)))
           (notes        (map tmtex-elsevier-auth-note
                              (tmtex-select-args-by-func 'author-note datas)))
           (emails       (map tmtex-elsevier-email
                              (tmtex-select-args-by-func 'author-email datas)))
           (urls         (map tmtex-elsevier-homepage
                              (tmtex-select-args-by-func
                                'author-homepage datas)))
           (affiliations (map tmtex-elsevier-affiliation
                              (tmtex-select-args-by-func
                                'author-affiliation datas)))
           (fnbeg        (1+ (- author-counter (length notes))))
           (fnend        (1+ author-counter))
           (fnref        (map number->string (.. fnbeg fnend)))
           (fnref        (map (lambda (x) (string-append "author-" x)) fnref))
           (names        (map (lambda (x)
                                (tmtex-elsevier-name  (cadr x) '() fnref))
                              (tmtex-select-args-by-func 'author-name datas))))
      `(!paragraph ,@names ,@affiliations ,@emails ,@urls ,@notes))))

(tm-define (tmtex-doc-data s l)
  (:mode elsevier-style?)

  (let* ((subtitles (map tmtex-elsevier-subtitle
                         (tmtex-select-args-by-func 'doc-subtitle l)))
         (notes     (map tmtex-elsevier-note
                         (tmtex-select-args-by-func 'doc-note l)))
         (miscs     (map tmtex-elsevier-misc
                         (tmtex-select-args-by-func 'doc-misc l)))
         (dates     (map tmtex-elsevier-date
                         (tmtex-select-args-by-func 'doc-date l)))
         (authors   (map tmtex-elsevier-author
                         (tmtex-select-args-by-func 'doc-author l)))
         ;; titles needed in last position due to side effects
         (titles    (map tmtex-elsevier-title
                         (tmtex-select-args-by-func 'doc-title l))))
    `(!document
        ,@titles
        ,@subtitles
        ,@notes
        ,@miscs
        ,@dates
        ,@authors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier clustered title macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-elsevier-clustered-author t author-notes)
  (if (or (npair? t) (npair? (cdr t)) (not (func? (cadr t) 'author-data))) '()
    (let* ((datas        (cdadr t))
           (author-notes (filter nnull? author-notes))
           (author-names (apply append
                                (map elsevier-split-authors
                                     (tmtex-select-args-by-func
                                       'author-name datas))))
           (names        (map tmtex (map elsevier-get-names author-names)))
           (names-refs   (map elsevier-get-name-refs author-names))
           (emails       (map tmtex-elsevier-email*
                              (tmtex-select-args-by-func
                                'author-email-note datas)))
           (urls         (map tmtex-elsevier-homepage*
                              (tmtex-select-args-by-func
                                'author-homepage-note datas)))
           (affiliations (map tmtex-elsevier-affiliation*
                              (tmtex-select-args-by-func
                                'author-affiliation-note datas)))
           (author-stuff (map
                           (lambda (auth)
                             (let* ((auref (list-ref names-refs author-counter))
                                    (afref (filter (lambda (x) (in? x auref))
                                                   (map car affiliations)))
                                    (fnref (filter (lambda (x) (in? x auref))
                                                   (map car author-notes)))
                                    (dummy (refstep-author)))
                               `(!paragraph
                                  ,(tmtex-elsevier-name auth afref fnref)
                                  ,@(map cadr
                                         (filter (lambda (email)
                                                   (in? (car email) auref))
                                                 emails))
                                  ,@(map cadr
                                         (filter (lambda (url)
                                                   (in? (car url) auref))
                                                 urls))))) names))
           (affiliations (map cadr affiliations))
           (author-notes (map cadr author-notes)))
      `((!paragraph ,@author-stuff)
        (!paragraph ,@affiliations)
        (!paragraph ,@author-notes)))))

(define (get-title-option l)
  (apply append (map cdr (tmtex-select-args-by-func 'doc-title-options l))))

(tm-define (tmtex-doc-data s l)
  (:mode elsevier-style?)
  (:require (or (in? "cluster-all" (get-title-option l))
                (in? "cluster-by-affiliation" (get-title-option l))))

  (let* ((sal       (add-notes (single-author-list (cons s l))))
         (subtitles  (map tmtex-elsevier-subtitle
                          (tmtex-select-args-by-func 'doc-subtitle l)))
         (notes      (map tmtex-elsevier-note
                          (tmtex-select-args-by-func 'doc-note l)))
         (miscs      (map tmtex-elsevier-misc
                          (tmtex-select-args-by-func 'doc-misc l)))
         (dates      (map tmtex-elsevier-date
                          (tmtex-select-args-by-func 'doc-date l)))
         (auth-notes (map tmtex-elsevier-auth-note*
                          (tmtex-select-args-by-func 'doc-footnote-text
                                                     (cdr sal))))
         (auth-stuff (apply append
                            (map (lambda (x)
                                   (tmtex-elsevier-clustered-author x auth-notes))
                                 (tmtex-select-args-by-func 'doc-author
                                                            (cdr sal)))))
         ;; titles needed in last position due to side effects
         (titles     (map tmtex-elsevier-title
                          (tmtex-select-args-by-func 'doc-title l))))
    `(!document
        ,@titles
        ,@subtitles
        ,@notes
        ,@miscs
        ,@dates
        ,@auth-stuff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elsevier abstract macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-abstract-data s l)
  (:mode elsevier-style?)
  (let* ((msc (tmtex-select-args-by-func 'abstract-msc l))
         (msc (apply append (map cdr (map tmtex msc))))
         (msc (list-intersperse msc '(!concat " " (sep) " ")))
         (msc (if (nnull? msc) `((!concat (PACS) " " ,@msc)) '()))
         (keywords (tmtex-select-args-by-func 'abstract-keywords l))
         (keywords (apply append (map cdr (map tmtex keywords))))
         (keywords (list-intersperse keywords '(!concat " " (sep) " ")))
         (keywords (if (nnull? keywords) `((!concat ,@keywords)) '()))
         (keywords (if (or (nnull? msc) (nnull? keywords))
                       `(((!begin "keyword")
                          (!document ,@keywords ,@msc))) '()))
         (abstract (map tmtex (tmtex-select-args-by-func 'abstract l))))
    `(!document ,@abstract ,@keywords)))

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
;; Customizations for JSC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-style-init body)
  (:mode jsc-style?)
  (init-elsevier body)
  (set! tmtex-packages (cons "natbib" tmtex-packages))
  (latex-set-packages '("amsthm" "yjsco" "natbib")))
