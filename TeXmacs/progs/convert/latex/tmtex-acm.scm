
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-acm.scm
;; DESCRIPTION : special conversions for acm styles
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-acm)
  (:use (convert latex tmtex)))

(tm-define (tmtex-transform-style x)
  (:mode acm-style?)
  (cond ((== x "acmconf") "acm_proc_article-sp")
        ((== x "sig-alternate") x)
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode acm-style?)
  (if (null? l) l
    (with n (number->string (length l))
      (set! l (list-intersperse
                (map cadr l) '(!concat (!linefeed) (alignauthor) (!linefeed))))
      `((!document (numberofauthors ,n)
                   (author (!indent (!concat (alignauthor) " " ,@l))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode acm-style?)
  (with names (tmtex-concat-Sep (map cadr names))
        `(author (!concat ,names
                          ,@urls
                          ,@notes
                          ,@miscs
                          ,@affiliations
                          ,@emails))))

(define (tmtex-make-title titles notes miscs)
  (with titles (tmtex-concat-Sep (map cadr titles))
        `(title (!concat ,titles ,@notes ,@miscs))))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes)
  (:mode acm-style?)
  `(!document
     ,(tmtex-make-title titles notes miscs)
     ,@subtitles 
     ,@(tmtex-append-authors authors)
     ,@dates
     (maketitle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (acm-line-break t)
  `(!concat (!nextline) ,t))

(tm-define (tmtex-doc-subtitle t)
  (:mode acm-style?)
  `(subtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(titlenote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode acm-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode acm-style?)
  (acm-line-break `(affaddr ,(tmtex (cadr t)))))

(tm-define (tmtex-author-email t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  (acm-line-break `(email ,(tmtex (cadr t)))))

(tm-define (tmtex-author-homepage t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmhomepage ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(titlenote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode acm-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define  (tmtex-make-abstract-data keywords msc abstract)
  (:mode acm-style?)
  `(!document ,@abstract ,@msc ,@keywords))

(tm-define (tmtex-abstract-keywords t)
  (:mode acm-style?)
  (with args (tmtex-concat-sep (map tmtex (cdr t)))
    `(keywords ,args)))

(tm-define (tmtex-abstract-msc t)
  (:mode acm-style?)
  `(category ,@(cdr t)))
