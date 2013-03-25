
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

(tm-define (tmtex-transform-style x)
  (:mode ieee-style?)
  (cond ((== x "ieeeconf") "IEEEconf")
        ((== x "ieeetran") "IEEEtran")
        (else x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEconf metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-append-authors l)
  (:mode ieee-style?)
  (if (null? l) l
    (with sep '(!concat (!linefeed) (and) (!linefeed))
      `((author (!indent (!concat ,@(list-intersperse (map cadr l) sep))))))))

(tm-define (tmtex-make-author names affiliations emails urls miscs notes)
  (:mode ieee-style?)
  (with names (tmtex-concat-Sep (map cadr names))
        `(author (!paragraph (!concat ,names ,@urls ,@notes)
                             ,@miscs
                             ,@affiliations
                             ,@emails))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IEEEconf specific titlemarkup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-author-affiliation t)
  (:mode ieee-style?)
  `((!begin "affiliation") ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (:mode ieee-style?)
  (set! t (tmtex-remove-line-feeds t))
  `(email ,(tmtex (cadr t))))
