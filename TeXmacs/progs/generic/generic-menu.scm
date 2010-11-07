
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-menu.scm
;; DESCRIPTION : default focus menu
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-menu)
  (:use (utils edit variants)
	(generic generic-edit)
	(generic format-edit)
	(generic format-geometry-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (variant-set t v)
  (tree-assign-node t v))

(define (variant-set-keep-numbering t v)
  (if (and (symbol-numbered? v) (symbol-unnumbered? (tree-label t)))
      (variant-set t (symbol-append v '*))
      (variant-set t v)))

(define (tag-menu-name l)
  (if (symbol-unnumbered? l)
      (tag-menu-name (symbol-drop-right l 1))
      (upcase-first (symbol->string l))))

(define (variant-menu-item v)
  (list (tag-menu-name v)
	(lambda () (variant-set-keep-numbering (focus-tree) v))))

(tm-define (variant-menu-items t)
  (with variants (variants-of (tree-label t))
    (map variant-menu-item variants)))

(tm-define (check-number? t)
  (tree-in? t (numbered-tag-list)))

(tm-define (number-toggle t)
  (when (numbered-context? t)
    (if (tree-in? t (numbered-tag-list))
	(variant-set t (symbol-append (tree-label t) '*))
	(variant-set t (symbol-drop-right (tree-label t) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding and removing children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-horizontal? t)
  (or (tree-is-dynamic? t)
      (and-with c (tree-down t)
	(tree-in? c '(table tformat)))))

(tm-define (structured-vertical? t)
  (or (tree-in? t '(tree))
      (and-with c (tree-down t)
	(tree-in? c '(table tformat)))))

(tm-define (focus-can-insert)
  (with t (focus-tree)
    (< (tree-arity t) (tree-maximal-arity t))))

(tm-define (focus-can-remove)
  (with t (focus-tree)
    (> (tree-arity t) (tree-minimal-arity t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Focus menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (opt cond? . l)
  `(if ,cond? (list ,@l) '()))

(tm-define (standard-focus-menu t)
  (append (opt #t
	       (cons* '-> (tag-menu-name (tree-label t))
		      (variant-menu-items t)))
	  (opt (numbered-context? t)
	       (list (list 'check "Numbered" "v"
			   (lambda () (check-number? (focus-tree))))
		     (lambda () (number-toggle (focus-tree)))))
	  (opt (toggle-context? t)
	       (list (list 'check "Unfolded" "v"
			   (lambda () (toggle-second-context? (focus-tree))))
		     (lambda () (toggle-toggle (focus-tree)))))
	  (opt (or (structured-horizontal? t) (structured-vertical? t))
	       (list '---))
          (opt (structured-vertical? t)
	       (list "Insert above"
		     (lambda () (structured-insert-up))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-insert
		     (list "Insert argument before"
			   (lambda () (structured-insert #f)))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-insert
		     (list "Insert argument after"
			   (lambda () (structured-insert #t)))))
          (opt (structured-vertical? t)
	       (list "Insert below"
		     (lambda () (structured-insert-down))))
          (opt (structured-vertical? t)
	       (list "Remove above"
		     (lambda () (structured-remove-up))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-remove
		     (list "Remove argument backwards"
			   (lambda () (structured-remove #f)))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-remove
		     (list "Remove argument forwards"
			   (lambda () (structured-remove #t)))))
          (opt (structured-vertical? t)
	       (list "Remove below"
		     (lambda () (structured-remove-down))))))


(tm-define (focus-menu)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-menu t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main focus icons bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-focus-icons t)
  (append (opt #t
	       (cons* '=>
		      (list 'balloon (tag-menu-name (tree-label t))
			    "Structured variant")
		      (variant-menu-items t)))
	  (opt (numbered-context? t)
	       (list (list 'balloon (list 'icon "tm_three.xpm")
			   "Toggle numbering")
		     (lambda () (number-toggle (focus-tree)))))
	  (opt (toggle-context? t)
	       (list (list 'balloon (list 'icon "tm_unfold.xpm")
			   "Fold / Unfold")
		     (lambda () (toggle-toggle (focus-tree)))))
	  (opt (or (structured-horizontal? t) (structured-vertical? t))
	       (list '|))
          (opt (structured-vertical? t)
	       (list (list 'balloon (list 'icon "tm_insert_up.xpm")
			   "Structured insert above")
		     (lambda () (structured-insert-up))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-insert
		     (list (list 'balloon (list 'icon "tm_insert_left.xpm")
				 "Structured insert at the left")
			   (lambda () (structured-insert #f)))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-insert
		     (list (list 'balloon (list 'icon "tm_insert_right.xpm")
				 "Structured insert at the right")
			   (lambda () (structured-insert #t)))))
          (opt (structured-vertical? t)
	       (list (list 'balloon (list 'icon "tm_insert_down.xpm")
			   "Structured insert below")
		     (lambda () (structured-insert-down))))
          (opt (structured-vertical? t)
	       (list (list 'balloon (list 'icon "tm_delete_up.xpm")
			   "Structured remove upwards")
		     (lambda () (structured-remove-up))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-remove
		     (list (list 'balloon (list 'icon "tm_delete_left.xpm")
				 "Structured remove leftwards")
			   (lambda () (structured-remove #f)))))
          (opt (structured-horizontal? t)
	       (list 'when focus-can-remove
		     (list (list 'balloon (list 'icon "tm_delete_right.xpm")
				 "Structured remove rightwards")
			   (lambda () (structured-remove #t)))))
          (opt (structured-vertical? t)
	       (list (list 'balloon (list 'icon "tm_delete_down.xpm")
			   "Structured remove downwards")
		     (lambda () (structured-remove-down))))))

(tm-define (texmacs-focus-icons)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-icons t))))
