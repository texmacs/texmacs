
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : textm.scm
;; DESCRIPTION : finalize LaTeX -> TeXmacs conversions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex textm)
  (:use (convert rewrite tmtm-eqns) (convert rewrite tmtm-tidy)))

(tm-define (textm-finalize l1)
  (let* ((l2 (tmtm-modernize-newlines l1))
	 (l3 (tmtm-nonumber->eqnumber l2))
	 (l4 (tmtm-eat-space-around-control l3))
	 (l5 (tmtm-remove-superfluous-newlines l4))
	 (l6 (tmtm-concat-document-correct l5)))
    l6))
