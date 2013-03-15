
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-ams.scm
;; DESCRIPTION : special conversions for AMS styles
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-ams)
  (:use (convert latex tmtex)))

(tm-define (tmtex-transform-style x)
  (:mode ams-style?) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS data preprocessing
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
  (:mode ams-style?)
  (cond ((stree-contains? doc 'abstract-data)
         (insert-maketitle-after doc 'abstract-data))
        ((stree-contains? doc 'doc-data)
         (insert-maketitle-after doc 'doc-data))
        (else doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode ams-style?)
  (with names (map (lambda (x) `(author ,x))
                   (list-intersperse (map cadr names) '(tmSep)))
        `(!paragraph ,@names
                     ,@affiliations
                     ,@emails
                     ,@urls
                     ,@notes
                     ,@miscs)))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes)
  (:mode ams-style?)
  `(!document
     (!paragraph ,@titles ,@subtitles ,@notes ,@miscs)
     ,@authors
     ,@dates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-doc-subtitle t)
  (:mode ams-style?)
  `(tmsubtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode ams-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode ams-style?)
  `(address ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(email ,(tmtex (cadr t))))

(tm-define (tmtex-author-homepage t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(urladdr ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode ams-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMS specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define  (tmtex-make-abstract-data keywords msc abstract)
  (:mode ams-style?)
  `(!document ,@abstract ,@msc ,@keywords))

(tm-define (tmtex-abstract-keywords t)
  (:mode ams-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (tmsep)))
    `(keywords (!concat ,@args))))

(tm-define (tmtex-abstract-msc t)
  (:mode ams-style?)
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (tmSep)))
    `(subjclass (!concat ,@args))))
