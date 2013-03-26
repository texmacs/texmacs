
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

(define (ieee-contains t l)
  (cond ((or (nlist? t) (null? t)) #f)
        ((in? (car t) l) #t)
        (else (or (ieee-contains (car t) l) (ieee-contains (cdr t) l)))))

(tm-define (tmtex-style-init doc)
  (:mode ieee-tran-style?)
  ;; ieeetran require to be in conference mode to print affiliations and emails
  (set! conference? (ieee-contains doc '(author-email author-affiliation))))

(tm-define (tmtex-transform-style x)
  (:mode ieee-style?)
  (cond ((== x "ieeeconf") "IEEEconf")
        ((and conference? (== x "ieeetran")) '("conference" "IEEEtran"))
        ((== x "ieeetran") "IEEEtran")
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEconf metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode ieee-conf-style?)
  (if (null? l) l
    (with sep '(!concat (!linefeed) (and) (!linefeed))
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode ieee-conf-style?)
  (with names (tmtex-concat-Sep (map cadr names))
        `(author (!paragraph (!concat ,names ,@urls ,@notes ,@miscs)
                             ,@affiliations
                             ,@emails))))

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
  (if (null? l) l
    (with sep '(!concat (!linefeed) "and~")
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-append-authors l)
  (:mode ieee-tran-style?)
  (:require conference?)
  (if (null? l) l
    (with sep '(!concat (!linefeed) (and) (!linefeed))
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode ieee-tran-style?)
  (:require conference?)
  (let* ((names (tmtex-concat-Sep (map cadr names)))
         (affiliations (map cadr affiliations))
         (authorblockA `(,@affiliations ,@emails))
         (authorblockA (list-intersperse authorblockA '(!nextline))))
    `(author (!paragraph
               (IEEEauthorblockN (!concat ,names ,@urls ,@notes ,@miscs))
               (IEEEauthorblockA (!concat ,@authorblockA))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEtran specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-author-affiliation t)
  (:mode ieee-tran-style?)
  (:require conference?)
  `(IEEEauthorblockA ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ieee-tran-style?)
  (:require conference?)
  (set! t (tmtex-remove-line-feeds t))
  `(tmieeeemail ,(tmtex (cadr t))))
