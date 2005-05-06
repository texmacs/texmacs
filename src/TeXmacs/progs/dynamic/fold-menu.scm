
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-menu.scm
;; DESCRIPTION : menus for folding
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-menu)
  (:use (dynamic fold-edit)))


(menu-bind insert-fold-menu
  ("Superpose" (make 'superpose))
  (if (style-has? "std-dtd")
      ---
      ("Folded" (make-fold))
      (if (or (inside? 'unfold)
	      (and (not (inside? 'unfold)) (not (inside? 'fold))))
	  (when (inside? 'unfold)
		("Fold" (fold))))
      (if (inside? 'fold)
	  ("Unfold" (unfold)))
      ---
      ("Switch" (make-switch))
      (when (inside? 'switch)
	    ("Add switch before" (switch-insert "before"))
	    ("Add switch after" (switch-insert "after"))
	    ("Remove this switch" (switch-remove "here"))
	    ---
	    (when (< 0 (switch-get-position))
		  ("Switch to previous" (switch-to "previous")))
	    (when (< (switch-get-position) (switch-get-last))
		  ("Switch to next" (switch-to "next")))
	    (when (< 0 (switch-get-position))
		  ("Switch to first" (switch-to "first")))
	    (when (< (switch-get-position) (switch-get-last))
		  ("Switch to last" (switch-to "last"))))))
