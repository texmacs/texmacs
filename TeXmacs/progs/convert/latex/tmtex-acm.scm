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
        ((== x "acmsmall") x)
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode acm-conf-style?)
  (set! l (filter nnull? l))
  (if (null? l) l
    (let* ((n (number->string (length l)))
           (sep '(!concat (!linefeed) (alignauthor) (!linefeed))))
      (set! l (list-intersperse (map cadr l) sep))
      `((!document (numberofauthors ,n)
                   (author (!indent (!concat (alignauthor) " " ,@l))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode acm-conf-style?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (result `(,@names ,@urls ,@notes ,@miscs ,@affiliations ,@emails)))
    (if (null? result) '()
      `(author (!concat ,@result)))))

(define (tmtex-make-title titles notes miscs)
  (let* ((titles (tmtex-concat-Sep (map cadr titles)))
         (result `(,@titles ,@notes ,@miscs)))
    (if (null? result) '()
      `((title (!concat ,@result))))))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                subtits-l dates-l miscs-l notes-l tr ar)
  (:mode acm-conf-style?)
  `(!document
     ,@(tmtex-make-title titles notes miscs)
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
  (:mode acm-conf-style?)
  `(subtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(titlenote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode acm-conf-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode acm-conf-style?)
    (with aff-lines
      (if (list>0? (cadr t))
        (map (lambda (x)
                (if (== x '(next-line))
                  '(!nextline)
                  `(affaddr ,(tmtex x))))
             (cdadr t))
        (if (null? (cdr t)) '() `((affaddr ,(tmtex (cadr t))))))
    (acm-line-break `(!concat ,@aff-lines))))

(tm-define (tmtex-author-email t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  (acm-line-break `(email ,(tmtex (cadr t)))))

(tm-define (tmtex-author-homepage t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmhomepage ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(titlenote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode acm-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmacmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define  (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (:mode acm-style?)
  (with result `(,@abstract ,@acm ,@arxiv ,@msc ,@pacs ,@keywords)
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-abstract-keywords t)
  (:mode acm-style?)
  (with args (tmtex-concat-sep (map tmtex (cdr t)))
    `(keywords ,@args)))

(tm-define (tmtex-abstract-acm t)
  (:mode acm-style?)
  (with l (cond ((== (length (cdr t)) 0) '("" "" ""))
                ((== (length (cdr t)) 1) (append (cdr t) '("" "")))
                ((== (length (cdr t)) 2) (append (cdr t) '("")))
                ((== (length (cdr t)) 3) (cdr t))
                (else (append (sublist (cdr t) 0 3)
                              `((!option ,(fourth (cdr t))))
                              (sublist (cdr t) 4 (length (cdr t))))))
    `(category ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACM specific misc markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-acm-conferenceinfo s l)
  (:mode acm-style?)
  `(conferenceinfo ,@(map tmtex l)))

(tm-define (tmtex-acm-copyright-year s l)
  (:mode acm-style?)
  `(CopyrightYear ,@(map tmtex l)))

(tm-define (tmtex-acm-crdata s l)
  (:mode acm-style?)
  `(crdata ,@(map tmtex l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACM specific macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:mode acm-style?)
  (qed #f))

(smart-table latex-texmacs-environment
  (:mode acm-style?)
  ("proof" #f))

(tm-define (tmtex-cite-detail s l)
  (:mode acm-style?)
  `(!concat ,(tex-apply 'cite (tmtex (car l)))
            " (" ,(tmtex (cadr l)) ")"))

(smart-table latex-texmacs-env-preamble
  (:mode acm-small-style?)
  ("theorem" #f)
  ("conjecture" #f)
  ("proposition" #f)
  ("lemma" #f)
  ("corollary" #f)
  ("definition" #f)
  ("remark" #f)
  ("example" #f))
