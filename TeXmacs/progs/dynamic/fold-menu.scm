
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
      (-> "Switch"
	  ("Alternatives" (make-switch 'switch))
	  ("Unroll" (make-switch 'unroll))
	  ("Expanded" (make-switch 'expanded)))
      (when (nnot (tree-innermost switch-context?))
	    ("Add switch before" (switch-insert-at :current))
	    ("Add switch after" (switch-insert-at :var-next))
	    ("Remove this switch" (switch-remove-at :current))
	    ---
	    (when (< 0 (switch-index))
		  ("Switch to first" (switch-to :first)))
	    (when (< 0 (switch-index))
		  ("Switch to previous" (switch-to :previous)))
	    (when (< (switch-index) (switch-index :last))
		  ("Switch to next" (switch-to :next)))
	    (when (< (switch-index) (switch-index :last))
		  ("Switch to last" (switch-to :last))))))
