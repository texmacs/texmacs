
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : textm.scm
;; DESCRIPTION : finalize LaTeX -> TeXmacs conversions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex textm)
  (:use
    (convert rewrite tmtm-eqns) (convert rewrite tmtm-tidy))
  (:export
    textm-finalize))

(define (textm-finalize l1)
  (let* ((l2 (tmtm-nonumber->eqnumber l1))
	 (l3 (tmtm-eat-space-around-control l2))
	 (l4 (tmtm-remove-superfluous-newlines l3)))
    l4))
