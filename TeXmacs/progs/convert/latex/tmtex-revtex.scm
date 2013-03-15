
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
        (else (or (stree-contains? (car t) u)
                  (in? #t (map (lambda (x) (stree-contains? x u)) (cdr t)))))))

(define (insert-maketitle-after t u)
  (cond ((nlist? t) t)
        ((== (car t) u) `(!document ,t (maketitle)))
        (else `(,(car t) ,@(map (lambda (x) (insert-maketitle-after x u))
                                (cdr t))))))

(tm-define (tmtex-style-preprocess doc)
  (:mode aip-style?)
  (revtex-set-style-option "aip")
  (cond ((stree-contains? doc 'abstract-data)
         (insert-maketitle-after doc 'abstract-data))
        ((stree-contains? doc 'doc-data)
         (insert-maketitle-after doc 'doc-data))
        (else doc)))

(tm-define (tmtex-style-preprocess doc)
  (:mode aps-style?)
  (if (stree-contains? doc 'abstract-keywords)
    (revtex-set-style-option "showkeys"))
  (if (stree-contains? doc 'abstract-msc)
    (revtex-set-style-option "showpacs"))
  (revtex-set-style-option "aps")
  (cond ((stree-contains? doc 'abstract-data)
         (insert-maketitle-after doc 'abstract-data))
        ((stree-contains? doc 'doc-data)
         (insert-maketitle-after doc 'doc-data))
        (else doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RevTeX metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode revtex-style?)
  (if (null? affiliations) (set! affiliations `((noaffiliation))))
  (with names (map (lambda (x) `(author ,x))
                   (list-intersperse (map cadr names) '(tmSep)))
        `(!paragraph ,@names
                     ,@emails
                     ,@urls
                     ,@notes
                     ,@miscs
                     ,@affiliations)))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes)
  (:mode revtex-style?)
  `(!document
     (!paragraph ,@titles ,@subtitles ,@notes ,@miscs)
     ,@authors
     ,@dates))

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
  `(affiliation ,(tmtex (cadr t))))

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

(tm-define  (tmtex-make-abstract-data keywords msc abstract)
  (:mode revtex-style?)
  `(!document ,@abstract ,@msc ,@keywords))

(tm-define (tmtex-abstract-keywords t)
  (:mode revtex-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (tmSep)))
    `(keywords (!concat ,@args))))

(tm-define (tmtex-abstract-msc t)
  (:mode revtex-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (tmsep)))
    `(pacs (!concat ,@args))))
