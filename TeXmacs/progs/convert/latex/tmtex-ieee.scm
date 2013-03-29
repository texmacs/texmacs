
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-ieee.scm
;; DESCRIPTION : special conversions for ieee styles
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven, François Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-ieee)
  (:use (convert latex tmtex)))

(define conference? #f)
(define clustered? #f)

(tm-define (tmtex-style-init doc)
  (:mode ieee-tran-style?)
  ;; ieeetran require to be in conference mode to print affiliations and emails
  (set! conference? (contains-tags? doc '(author-email author-affiliation)))
  (set! clustered?
    (and
      conference?
      (or
        (contains-stree? doc '(doc-title-options "cluster-all"))
        (contains-stree? doc '(doc-title-options "cluster-by-affiliation"))))))

(tm-define (tmtex-transform-style x)
  (:mode ieee-style?)
  (cond ((== x "ieeeconf") "IEEEconf")
        ((and (or clustered? conference?) (== x "ieeetran"))
         '("conference" "IEEEtran"))
        ((== x "ieeetran") "IEEEtran")
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEconf metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode ieee-conf-style?)
  (set! l (filter nnull? l))
  (if (null? l) l
    (with sep '(!concat (!linefeed) (and) (!linefeed))
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode ieee-conf-style?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (result `(,@names ,@urls ,@notes ,@miscs))
         (result (if (null? result) '() `((!concat ,@result))))
         (result `(,@result ,@affiliations ,@emails)))
    (if (null? result) '() `(author (!paragraph ,@result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEconf specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-author-affiliation t)
  (:mode ieee-conf-style?)
  `((!begin "affiliation") ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ieee-conf-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(email ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEtran metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode ieee-tran-style?)
  (set! l (filter nnull? l))
  (if (null? l) l
    (with sep '(!concat (!linefeed) "and~")
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-append-authors l)
  (:mode ieee-tran-style?)
  (:require conference?)
  (set! l (filter nnull? l))
  (if (null? l) l
    (with sep '(!concat (!linefeed) (and) (!linefeed))
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode ieee-tran-style?)
  (:require conference?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (affiliations (map cadr affiliations))
         (authorblockN `(,@names ,@urls ,@notes ,@miscs))
         (authorblockN (if (null? authorblockN) '()
                         `((IEEEauthorblockN (!concat ,@authorblockN)))))
         (authorblockA `(,@affiliations ,@emails))
         (authorblockA (list-intersperse authorblockA '(!nextline)))
         (authorblockA (if (null? authorblockA) '()
                         `((IEEEauthorblockA (!concat ,@authorblockA))))))
    (if (and (null? authorblockN) (null? authorblockA)) '()
      `(author (!paragraph ,@authorblockN ,@authorblockA)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEtran clustered metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-prepare-doc-data l)
  (:mode ieee-tran-style?)
  (:require clustered?)
  (set! l (map tmtex-replace-documents l))
  (set! l (make-references l 'author-affiliation #t #t))
  (set! l (make-references l 'author-email #t #t))
  l)

(tm-define (tmtex-append-authors l)
  (:mode ieee-tran-style?)
  (:require clustered?)
  (set! l (filter nnull? l))
  (if (null? l) ()
    (let* ((sep   '(!concat (!linefeed)))
           (names (map (lambda (au)
                         (filter (lambda (x)
                                   (== (car x) 'IEEEauthorblockN)) au)) l))
           (names (map car (filter nnull? names)))
           (names (tmtex-concat-sep (map cadr names)))
           (l*    (map (lambda (au)
                         (filter (lambda (x)
                                   (!= (car x) 'IEEEauthorblockN)) au)) l))
           (l*    (filter nnull? l*))
           (l*    (apply append l*))
           (names (if (null? names) '() `((IEEEauthorblockN ,@names))))
           (r     `(,@names ,@l*)))
      `((author (!indent (!concat ,@(list-intersperse r sep))))))))

(tm-define (tmtex-make-author names affs emails urls miscs notes
                              affs-l emails-l urls-l miscs-l notes-l)
  (:mode ieee-tran-style?)
  (:require clustered?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (authorblockN `(,@names ,@affs ,@emails ,@urls ,@notes ,@miscs))
         (authorblockN (if (null? authorblockN) '()
                         `((IEEEauthorblockN (!concat ,@authorblockN)))))
         (authorblockA `(,@affs-l ,@emails-l))
         (authorblockA (map (lambda (x) `(IEEEauthorblockA ,x)) authorblockA)))
    (if (and (null? authorblockN) (null? authorblockA)) '()
      `(,@authorblockN ,@authorblockA))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEtran specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-author-affiliation-ref s l)
  `(IEEEauthorrefmark ,(car l)))

(tm-define (tmtex-author-affiliation-label s l)
  `(!concat (IEEEauthorrefmark ,(car l))
                               ,(tmtex (cadr l))))
(tm-define (tmtex-author-email-ref s l)
  `(IEEEauthorrefmark ,(car l)))

(tm-define (tmtex-author-email-label s l)
  `(!concat (IEEEauthorrefmark ,(car l))
                               ,(tmtex-author-email l)))

(tm-define (tmtex-author-affiliation t)
  (:mode ieee-tran-style?)
  (:require conference?)
  `(IEEEauthorblockA ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ieee-tran-style?)
  (:require conference?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmieeeemail ,(tmtex (cadr t))))
