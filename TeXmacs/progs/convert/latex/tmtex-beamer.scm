
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-beamer.scm
;; DESCRIPTION : special conversions for Beamer style
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-beamer)
  (:use (convert latex tmtex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-transform-style x)
  (:mode beamer-style?)
  "beamer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer document preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-transform l what by)
  (cond ((or (null? l) (nlist? l)) l)
        ((== (car l) what) `(,by ,@(cdr l)))
        (else
          (map (lambda (x) (stree-transform x what by)) l))))

(define (beamer-make-slides doc)
  (set! doc (stree-transform doc 'hidden 'slide))
  (stree-transform doc 'shown 'slide))

(tm-define (tmtex-style-preprocess doc)
  (:mode beamer-style?)
  (beamer-make-slides doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode beamer-style?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (result `(,@names ,@urls ,@notes ,@miscs)))
    (if (null? result) '() `(author (!paragraph ,@result)))))

(define (beamer-append in l)
  (set! l (filter nnull? l))
  (if (< (length l) 1) l
    (with lf `(!concat (!linefeed) (and) (!linefeed))
      `((,in
          (!indent (!concat ,@(list-intersperse (map cadr l) lf))))))))

(define (svjour-make-title titles notes miscs)
  (let* ((titles (tmtex-concat-Sep (map cadr titles)))
         (result `(,@titles ,@notes ,@miscs)))
    (if (null? result) '()
      `((title (!concat ,@result))))))

(define (svjour-make-doc-data titles subtits authors affs dates miscs notes tr ar)
  `(!document
     ,@(svjour-make-title titles notes miscs)
     ,@subtits
     ,@tr
     ,@ar
     ,@(beamer-append 'author authors)
     ,@(beamer-append 'institute affs)
     ,@dates
     (maketitle)))

(tm-define (tmtex-doc-data s l)
  (:mode beamer-style?)
  (set! l (map tmtex-replace-documents l))
  (let* ((subtitles (map tmtex-doc-subtitle
                         (tmtex-select-args-by-func 'doc-subtitle l)))
         (notes     (map tmtex-doc-note
                         (tmtex-select-args-by-func 'doc-note l)))
         (miscs     (map tmtex-doc-misc
                         (tmtex-select-args-by-func 'doc-misc l)))
         (dates     (map tmtex-doc-date
                         (tmtex-select-args-by-func 'doc-date l)))
         (authors   (map tmtex-doc-author
                         (tmtex-select-args-by-func 'doc-author l)))
         (ar        (map tmtex-doc-running-author
                         (tmtex-select-args-by-func 'doc-running-author l)))
         (titles    (map tmtex-doc-title
                         (tmtex-select-args-by-func 'doc-title l)))
         (tr        (map tmtex-doc-running-title
                         (tmtex-select-args-by-func 'doc-running-title l)))
         (affs      (map tmtex-affiliation-group
                         (cluster-by-affiliations
                           (tmtex-select-args-by-func 'doc-author l)))))
    (svjour-make-doc-data titles subtitles authors affs dates miscs notes tr ar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer affiliation clustering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (beamer-clear-aff aff a filter?)
  (with datas (cdadr a)
    (if (and filter?
             (== `(,aff)
                 (filter (lambda (x) (== 'author-affiliation (car x))) datas)))
      '()
      `(doc-author (author-data ,@(filter (lambda (x) (!= aff x)) datas))))))

(define (next-affiliation l)
  (cond ((or (null? l) (nlist? l)) #f)
        ((in?   (car l) '(doc-author author-data)) (next-affiliation (cdr l)))
        ((==    (car l) 'author-affiliation) l)
        ((list? (car l))
         (with na (next-affiliation (car l))
           (if na na (next-affiliation (cdr l)))))
        (else #f)))

(define (cluster-by-affiliations l)
  (if (nlist? l) l
    (let* ((aff     (next-affiliation l))
           (hasaff  (filter (lambda (x)
                              (or (not aff)
                                  (and (list? x) (list? (cdr x))
                                       (list? (cadr x))
                                       (in? aff (cdadr x))))) l))
           (hasaff* (map (lambda (x) (beamer-clear-aff aff x #f)) hasaff))
           (l*      (map (lambda (x) (beamer-clear-aff aff x #t)) l))
           (l*      (filter nnull? l*))
           (aff*    `(affiliation-group
                       ,(if aff (cadr aff) '()) ,@hasaff*)))
      (if aff (append `(,aff*) (cluster-by-affiliations l*)) `(,aff*)))))

(tm-define (tmtex-affiliation-group t)
  (with old-tmtex-make-author (eval tmtex-make-author)
    (set! tmtex-make-author
      (lambda (names affiliations emails urls miscs notes
                     affs-l emails-l urls-l miscs-l notes-l)
        (with names (tmtex-concat-Sep (map cadr names))
          (cond ((and (null? names) (null? emails)) '())
                ((or (null? names) (null? emails))
                 `(!concat ,@names)) ;hack
                (else `(!concat ,@names)))))) ;hack
    (let* ((affs     (cadr t))
           (affs     (if (null? affs) '()
                       `((!concat (!linefeed)(at)(!linefeed) ,(tmtex affs)))))
           (auth-sep '(!concat " " (and) " "))
           (authors  (map tmtex-doc-author (cddr t)))
           (authors  (list-intersperse authors auth-sep)))
      (set! tmtex-make-author (eval old-tmtex-make-author))
      (if (and (null? authors) (null? affs)) '()
        `(institute (!concat ,@authors ,@affs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-doc-running-title t)
  (:mode beamer-style?)
  `(titlerunning ,(tmtex (cadr t))))

(tm-define (tmtex-doc-subtitle t)
  (:mode beamer-style?)
  `(subtitle ,(tmtex (cadr t))))

(tm-define (tmtex-doc-note t)
  (:mode beamer-style?)
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (:mode beamer-style?)
  `(tmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  (:mode beamer-style?)
  `(date ,(tmtex (cadr t))))

(tm-define (tmtex-doc-running-author t)
  (:mode beamer-style?)
  `(authorrunning ,(tmtex (cadr t))))

(tm-define (tmtex-author-affiliation t)
  (:mode beamer-style?)
  `(institute ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode beamer-style?)
  `(email ,(tmtex (cadr t))))

(tm-define (tmtex-author-homepage t)
  (:mode beamer-style?)
  `(tmfnhomepage ,(tmtex (cadr t))))

(tm-define (tmtex-author-note t)
  (:mode beamer-style?)
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (:mode beamer-style?)
  `(tmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer specific abstract markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define  (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (:mode beamer-style?)
  (:require (not llncs?))
  (with result `(,@abstract ,@arxiv ,@acm ,@msc ,@pacs ,@keywords)
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-abstract-keywords t)
  (:mode beamer-style?)
  (:require (not llncs?))
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (and)))
    `(keywords (!concat ,@args))))

(tm-define (tmtex-abstract-msc t)
  (:mode beamer-style?)
  (:require (not llncs?))
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (and)))
    `(subclass (!concat ,@args))))

(tm-define (tmtex-abstract-acm t)
  (:mode beamer-style?)
  (:require (not llncs?))
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (and)))
    `(CRclass (!concat ,@args))))

(tm-define (tmtex-abstract-pacs t)
  (:mode beamer-style?)
  (:require (not llncs?))
  (with args (list-intersperse (map tmtex (cdr t)) '(!group (and)))
    `(PACS (!concat ,@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beamer specific frame markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-beamer-slide s l)
  (:mode beamer-style?)
  `((!begin "frame") ,(tmtex (car l))))

(tm-define (tmtex-beamer-tit s l)
  (:mode beamer-style?)
  `(frametitle ,(tmtex (car l))))
