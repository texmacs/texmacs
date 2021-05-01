
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-revtex.scm
;; DESCRIPTION : special conversions for RevTeX styles
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-revtex)
  (:use (convert latex tmtex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define revtex-style '("revtex4-1"))
(define revtex-clustered? #f)

(tm-define (tmtex-style-init body)
  (:mode revtex-style?)
  (set! revtex-style '("revtex4-1"))
  (set! revtex-clustered? #f))

(define (revtex-set-style-option s)
    (set! revtex-style (append (list s) revtex-style)))

(tm-define (tmtex-transform-style x)
  (:mode revtex-style?) revtex-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX data preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-contains? t u)
  (cond ((== t u) #t)
        ((nlist? t) #f)
        ((null? t) #f)
        (else (in? #t (map (lambda (x) (stree-contains? x u)) t)))))

(define (insert-maketitle-after t u)
  (cond ((nlist? t) t)
        ((== (car t) u) `(!document ,t (maketitle)))
        (else `(,(car t) ,@(map (lambda (x) (insert-maketitle-after x u))
                                (cdr t))))))
(define (revtex-style-preprocess doc)
  (cond ((stree-contains? doc 'abstract-data)
         (insert-maketitle-after doc 'abstract-data))
        ((stree-contains? doc 'doc-data)
         (insert-maketitle-after doc 'doc-data))
        (else doc)))

(tm-define (tmtex-style-preprocess doc)
  (:mode aip-style?)
  (revtex-set-style-option "aip")
  (revtex-set-style-option "reprint")
  (revtex-style-preprocess doc))

(tm-define (tmtex-style-preprocess doc)
  (:mode aps-style?)
  (if (stree-contains? doc 'abstract-keywords)
    (revtex-set-style-option "showkeys"))
  (if (stree-contains? doc 'abstract-msc)
    (revtex-set-style-option "showpacs"))
  (revtex-set-style-option "aps")
  (revtex-set-style-option "reprint")
  (revtex-style-preprocess doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode revtex-style?)
  (if (and (not revtex-clustered?) (null? affiliations))
    (set! affiliations `((noaffiliation))))
  (let* ((names (map (lambda (x) `(author ,x))
                   (list-intersperse (map cadr names) '(tmSep))))
         (result `(,@names ,@emails ,@urls ,@notes ,@miscs ,@affiliations)))
    (if (null? result) '() `(!paragraph ,@result))))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                subtits-l dates-l miscs-l notes-l tr ar)
  (:mode revtex-style?)
  (let* ((title-data `(,@titles ,@subtitles ,@notes ,@miscs))
         (title-data (if (null? title-data) '() `((!paragraph ,@title-data))))
         (authors*   (filter pair? authors)))
    (if (and (null? title-data) (null? authors*) (null? dates)) '()
        `(!document ,@title-data ,@authors* ,@dates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX clustered authors presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-with tags l)
  (if (null? l) '()
    (letrec ((remove-tag
               (lambda (x)
                 (let* ((root  (car x))
                        (args  (cdr x))
                        (args* (filter (lambda (y) (nin? y tags)) args)))
                   `(,root ,@args*)))))
      (let* ((last    (cAr l))
             (others  (cDr l))
             (others* (map remove-tag others)))
        (if (null? tags)
          (set! last `(,(car last)
                       ,@(cdr last) (author-affiliation (noaffiliation)))))
        (map (lambda (x) `(doc-author ,x)) (append others* (list last)))))))

(define (cluster-by tag l)
  (if (or (null? l) (nlist? (car l))) '()
      (letrec ((get-affiliations
                (lambda (x) (tmtex-select-args-by-func tag x))))
        (let* ((author (car l))
               (aff    (get-affiliations author))
               (same   (filter (lambda (x) (== aff (get-affiliations x))) l))
               (others (filter (lambda (x) (!= aff (get-affiliations x))) l)))
          (append (merge-with aff same) (cluster-by tag others))))))

(tm-define (tmtex-doc-data s l)
  (:mode revtex-style?)
  (:require (or revtex-clustered?
                (stree-contains?  l '(doc-title-options "cluster-all"))
                (stree-contains?  l '(doc-title-options
                                       "cluster-by-affiliation"))))
  (if (not revtex-clustered?) (set! revtex-clustered? #t))
  (set! l (map tmtex-replace-documents l))
  (let* ((subtitles (map tmtex-doc-subtitle
                         (tmtex-select-args-by-func 'doc-subtitle l)))
         (notes     (map tmtex-doc-note
                         (tmtex-select-args-by-func 'doc-note l)))
         (miscs     (map tmtex-doc-misc
                         (tmtex-select-args-by-func 'doc-misc l)))
         (dates     (map tmtex-doc-date
                         (tmtex-select-args-by-func 'doc-date l)))
         (titles    (map tmtex-doc-title
                         (tmtex-select-args-by-func 'doc-title l)))
         (authors   (map cadr
                         (tmtex-select-args-by-func 'doc-author l)))
         (authors   `((!document ,@(map tmtex-doc-author
                                        (cluster-by
                                          'author-affiliation authors))))))
    (with r (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                 '() '() '() '() '() '())
    (set! revtex-clustered? #f)
    r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-doc-subtitle t)
  (:mode revtex-style?)
  `(tmsubtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode revtex-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode revtex-style?)
  (if (== t '(author-affiliation (noaffiliation)))
    '(noaffiliation)
    `(affiliation ,(tmtex (cadr t)))))

(tm-define (tmtex-author-email t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(email (!option "Email: ") ,(tmtex (cadr t))))

(tm-define (tmtex-author-homepage t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(homepage (!option "Web: ") ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode revtex-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-in-abstract what in)
  (if (null? in)
    (if (null? what) '() `(((!begin "abstract") (document ,@what))))
    `(((!begin "abstract") (!document ,@(map cadr in) ,@what)))))

(tm-define  (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (:mode revtex-style?)
  (with class `(,@acm ,@arxiv ,@msc)
    (set! abstract (move-in-abstract class abstract)))
  (with result `(,@abstract ,@pacs ,@keywords)
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-abstract-keywords t)
  (:mode revtex-style?)
  (with args (tmtex-concat-sep (map tmtex (cdr t)))
    `(keywords ,@args)))

(tm-define (tmtex-abstract-pacs t)
  (:mode revtex-style?)
  (with args (tmtex-concat-sep (map tmtex (cdr t)))
    `(pacs ,@args)))
