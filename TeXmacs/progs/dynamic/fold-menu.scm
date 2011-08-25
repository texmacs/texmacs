
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for direct folding and switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (when (switch-index (focus-tree))
      (when (< 0 (switch-index (focus-tree)))
        ("Switch to first" (dynamic-first)))
      (when (< 0 (switch-index (focus-tree)))
        ("Switch to previous" (dynamic-previous)))
      (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
        ("Switch to next" (dynamic-next)))
      (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
        ("Switch to last" (dynamic-last))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting foldable and switchable tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-fold-menu
  (-> "Folded"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Documentation" (make-toggle 'folded-documentation))
      ("Grouped" (make-toggle 'folded-grouped))
      ;;---
      ;;(link fold-menu)
      )
  (-> "Summarized"
      ("Default" (make-toggle 'summarized))
      ---
      ("Plain" (make-toggle 'summarized-plain))
      ("Standard" (make-toggle 'summarized-std))
      ("Environment" (make-toggle 'summarized-env))
      ("Documentation" (make-toggle 'summarized-documentation))
      ("Grouped" (make-toggle 'summarized-grouped))
      ;;---
      ;;(link fold-menu)
      )
  (-> "Switch"
      ("Standard" (make-switch 'switch))
      (when (not (screens-buffer?))
        ("Screens" (make-screens)))
      ("Tiny" (make-switch 'tiny-switch))
      ;;---
      ;;(link switch-menu)
      )
  (-> "Unroll"
      ("Standard" (make-switch 'unroll))
      ;;---
      ;;(link switch-menu)
      )
  (-> "Expanded"
      ("Standard" (make-switch 'expanded))
      ("Slides" (make-switch 'slides))
      ;;---
      ;;(link switch-menu)
      )
  (-> "Traversal"
      ("Fold back" (make 'fold-back))
      ("Keep unfolded" (make 'keep-unfolded))
      (if #f
	  ("Animate folding" (noop))
	  ("Animate unfolding" (noop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operate on buffers with dynamic markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(menu-bind dynamic-menu
  ("First" (dynamic-operate-on-buffer :first))
  (if (in-screens?)
      ("Previous screen" (screens-switch-to :previous)))
  ("Previous" (dynamic-traverse-buffer :previous))
  ("Next" (dynamic-traverse-buffer :next))
  (if (in-screens?)
      ("Next screen" (screens-switch-to :next)))
  ("Last" (dynamic-operate-on-buffer :last))
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

(menu-bind dynamic-icons
  ((balloon (icon "tm_larrow_bar.xpm") "First")
   (dynamic-operate-on-buffer :first))
  (if (in-screens?)
      ((balloon (icon "tm_larrow_double.xpm") "Previous screen")
       (screens-switch-to :previous)))
  ((balloon (icon "tm_larrow.xpm") "Previous")
   (dynamic-traverse-buffer :previous))
  ((balloon (icon "tm_rarrow.xpm") "Next")
   (dynamic-traverse-buffer :next))
  (if (in-screens?)
      ((balloon (icon "tm_rarrow_double.xpm") "Next screen")
       (screens-switch-to :next)))
  ((balloon (icon "tm_rarrow_bar.xpm") "Last")
   (dynamic-operate-on-buffer :last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propose insertion of 'screens' tag in beamer style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (screens-buffer?)
  (with t (buffer-tree)
    (and (tree-is? t 'document)
         (tree-is? t :last 'screens))))

(tm-define (make-screens)
  (let* ((t (buffer-tree))
         (l (tree-children t))
         (p (cursor-inside? t)))
    (if (and (tree-is? t 'document)
             (tree-in? t 0 '(hide-preamble show-preamble)))
        (begin
          (tree-assign! t `(document ,(tree-ref t 0)
                                     (screens (shown (document ,@(cdr l))))))
          (if (!= (car p) 0)
              (apply tree-go-to `(,t 1 0 0 ,(- (car p) 1) ,@(cdr p)))))
        (begin
          (tree-assign! t `(document (screens (shown (document ,@l)))))
          (apply tree-go-to `(,t 0 0 0 ,@p))))))

(tm-define (document-propose-screens?)
  (and (style-has? "beamer-style")
       (not (screens-buffer?))))

(tm-menu (focus-document-extra-menu t)
  (:require (document-propose-screens?))
  ("Screens" (make-switch 'screens)))

(tm-menu (focus-document-extra-icons t)
  (:require (document-propose-screens?))
  (minibar
    ((balloon "Screens" "Make a multi-slide presentation")
     (make-screens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slide titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (slide-propose-title? t)
  (and-with u (tree-ref t :down :down)
    (and (tree-is? u 'document)
         (not (tree-is? u 0 'tit)))))

(tm-define (slide-insert-title t)
  (with u (tree-ref t :down :down)
    (tree-insert u 0 '((tit "")))
    (tree-go-to u 0 0 0)))

(tm-define (search-slide-name t)
  (cond ((tree-in? t '(shown hidden document))
         (search-slide-name (tree-ref t 0)))
        ((tree-is? t 'tit)
         (texmacs->string (tm-ref t 0)))
        (else "")))

(tm-define (get-slide-name t i)
  (with s (search-slide-name t)
    (string-append "Slide " (number->string (+ i 1))
                   (if (== s "") "" (string-append ": " s)))))

(tm-menu (focus-slides-menu t)
  (for (i (.. 0 (tree-arity t)))
    ((eval (get-slide-name (tree-ref t i) i))
     (switch-to t i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus shen focus is on 'screens' tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  (:require (tree-is? t 'screens))
  #f)

(tm-menu (standard-focus-menu t)
  (:require (tree-is? t 'screens))
  (dynamic (focus-style-menu t))
  ---
  (dynamic (focus-tag-menu t))
  ---
  (dynamic (focus-insert-menu t))
  ---
  (dynamic (focus-slides-menu t))
  (assuming (slide-propose-title? t)
    ---
    ("Title" (slide-insert-title t))))

(tm-menu (standard-focus-icons t)
  (:require (tree-is? t 'screens))
  (dynamic (focus-style-icons t))  
  (glue #f #f 5 0)
  (minibar (dynamic (focus-insert-icons t)))
  (glue #f #f 5 0)
  (minibar (dynamic (focus-tag-icons t)))
  (glue #f #f 5 0)
  (with i (tree-index (tree-down t))
    (mini #t
      (=> (eval (get-slide-name (tree-ref t i) i))
          (dynamic (focus-slides-menu t)))))
  (assuming (slide-propose-title? t)
    (glue #f #f 5 0)
    (minibar
     ((balloon "Title" "Insert title") (slide-insert-title t)))))
