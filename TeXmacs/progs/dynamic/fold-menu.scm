
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-menu.scm
;; DESCRIPTION : menus for folding
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
    ("Add branch before" (switch-insert-at (focus-tree) :current))
    ("Add branch after" (switch-insert-at (focus-tree) :var-next))
    ("Remove this branch" (switch-remove-at (focus-tree) :current))
    ---
    (when (< 0 (switch-index (focus-tree)))
      ("Switch to first" (dynamic-first)))
    (when (< 0 (switch-index (focus-tree)))
      ("Switch to previous" (dynamic-previous)))
    (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
      ("Switch to next" (dynamic-next)))
    (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
      ("Switch to last" (dynamic-last)))))

(define-menu (fold/unfold-menu-entry x which action)
  (with sym (string->symbol x)
    (when (ahash-ref which sym)
      ((eval (upcase-first x))
       (dynamic-operate-on-buffer (list action sym))))))

(tm-menu (fold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (assuming (nnull? l) ---)
    (for (x l) (dynamic (fold/unfold-menu-entry x second :fold)))))

(tm-menu (unfold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (assuming (nnull? l) ---)
    (for (x l) (dynamic (fold/unfold-menu-entry x second :unfold)))))

(menu-bind insert-fold-menu
  ("First" (dynamic-operate-on-buffer :first))
  ("Previous" (dynamic-traverse-buffer :previous))
  ("Next" (dynamic-traverse-buffer :next))
  ("Last" (dynamic-operate-on-buffer :last))
  ---
  (-> "Folded"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Documentation" (make-toggle 'folded-documentation))
      ("Grouped" (make-toggle 'folded-grouped))
      ---
      (link fold-menu))
  (-> "Summarized"
      ("Default" (make-toggle 'summarized))
      ---
      ("Plain" (make-toggle 'summarized-plain))
      ("Standard" (make-toggle 'summarized-std))
      ("Environment" (make-toggle 'summarized-env))
      ("Documentation" (make-toggle 'summarized-documentation))
      ("Grouped" (make-toggle 'summarized-grouped))
      ---
      (link fold-menu))
  (-> "Switch"
      ("Standard" (make-switch 'switch))
      ("Screens" (make-switch 'screens))
      ("Tiny" (make-switch 'tiny-switch))
      ---
      (link switch-menu))
  (-> "Unroll"
      ("Standard" (make-switch 'unroll))
      ---
      (link switch-menu))
  (-> "Expanded"
      ("Standard" (make-switch 'expanded))
      ("Slides" (make-switch 'slides))
      ---
      (link switch-menu))
  (-> "Traversal"
      ("Fold back" (make 'fold-back))
      ("Keep unfolded" (make 'keep-unfolded))
      (when #f
	("Animate folding" (noop))
	("Animate unfolding" (noop))))
  ---
  (-> "Fold"
      ("All" (dynamic-operate-on-buffer :fold))
      (link fold-environments-menu))
  (-> "Unfold"
      ("All" (dynamic-operate-on-buffer :unfold))
      (link unfold-environments-menu))
  (-> "Compress"
      ("Preserve tags" (dynamic-operate-on-buffer :compress))
      ("Change tags" (dynamic-operate-on-buffer :var-compress)))
  (-> "Expand"
      ("Preserve tags" (dynamic-operate-on-buffer :expand))
      ("Change tags" (dynamic-operate-on-buffer :var-expand))
      ("Make slides" (dynamic-make-slides))))

(tm-define (alternate-second-name t)
  (:require (fold-context? t))
  "Unfold")

(tm-define (alternate-second-icon t)
  (:require (fold-context? t))
  "tm_alternate_both.xpm")
