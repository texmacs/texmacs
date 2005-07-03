
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

(menu-bind fold-menu
  (when (with t (tree-innermost dynamic-context?)
	  (and t (toggle-second-context? t)))
    ("Fold" (dynamic-previous)))
  (when (with t (tree-innermost dynamic-context?)
	  (and t (toggle-first-context? t)))
    ("Unfold" (dynamic-next))))

(menu-bind switch-menu
  (when (with t (tree-innermost dynamic-context?)
	  (and t (switch-context? t)))
    ("Add branch before" (switch-insert-at :current))
    ("Add branch after" (switch-insert-at :var-next))
    ("Remove this branch" (switch-remove-at :current))
    ---
    (when (< 0 (switch-index))
      ("Switch to first" (dynamic-first)))
    (when (< 0 (switch-index))
      ("Switch to previous" (dynamic-previous)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to next" (dynamic-next)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to last" (dynamic-last)))))

(menu-bind insert-fold-menu
  ("First" (dynamic-operate-on-buffer :first))
  ("Previous" (dynamic-traverse-buffer #f))
  ("Next" (dynamic-traverse-buffer #t))
  ("Last" (dynamic-operate-on-buffer :last))
  ---
  (-> "Folded"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Grouped" (make-toggle 'folded-bracket))
      ---
      (link fold-menu))
  (-> "Summarized"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Grouped" (make-toggle 'folded-bracket))
      ---
      (link fold-menu))
  (-> "Switch"
      ("New" (make-switch 'switch))
      ---
      (link switch-menu))
  (-> "Unroll"
      ("New" (make-switch 'unroll))
      ---
      (link switch-menu))
  (-> "Expanded"
      ("New" (make-switch 'expanded))
      ---
      (link switch-menu))
  ---
  (-> "Compress"
      ("Fold" (dynamic-operate-on-buffer :fold))
      ("Compress" (dynamic-operate-on-buffer :compress)))
  (-> "Expand"
      ("Unfold" (dynamic-operate-on-buffer :unfold))
      ("Expand" (dynamic-operate-on-buffer :expand))))
