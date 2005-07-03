
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
  (-> "Insert toggle"
      (group "Folded")
      ("Default" (make-toggle 'folded))
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Grouped" (make-toggle 'folded-bracket))
      ---
      (group "Summarized")
      ("Default" (make-toggle 'summarized))
      ("Plain" (make-toggle 'summarized-plain))
      ("Standard" (make-toggle 'summarized-std))
      ("Environment" (make-toggle 'summarized-env))
      ("Grouped" (make-toggle 'summarized-bracket)))
  (when (nnot (tree-innermost toggle-second-context?))
    ("Fold" (fold)))
  (when (nnot (tree-innermost toggle-first-context?))
    ("Unfold" (unfold)))
  ---
  (-> "Insert switch"
      ("Alternatives" (make-switch 'switch))
      ("Unroll" (make-switch 'unroll))
      ("Expanded" (make-switch 'expanded)))
  (when (nnot (tree-innermost switch-context?))
    (-> "Branch"
	("Add branch before" (switch-insert-at :current))
	("Add branch after" (switch-insert-at :var-next))
	("Remove this branch" (switch-remove-at :current)))
    (when (< 0 (switch-index))
      ("Switch to first" (switch-to :first :start)))
    (when (< 0 (switch-index))
      ("Switch to previous" (switch-to :previous :end)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to next" (switch-to :next :start)))
    (when (< (switch-index) (switch-index :last))
      ("Switch to last" (switch-to :last :end))))
  ---
  ("First" (dynamic-operate-on-buffer :first))
  ("Previous" (dynamic-traverse-buffer #f))
  ("Next" (dynamic-traverse-buffer #t))
  ("Last" (dynamic-operate-on-buffer :last))
  ---
  ("Fold all" (dynamic-operate-on-buffer :fold))
  ("Unfold all" (dynamic-operate-on-buffer :unfold))
  ("Compress all" (dynamic-operate-on-buffer :compress))
  ("Expand all" (dynamic-operate-on-buffer :expand)))
