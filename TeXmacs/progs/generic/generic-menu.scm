
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
;; Special handles for images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-input-handle t i w)
  (if (tree-atomic? (tree-ref t i))
      (list 'input
	    (lambda (answer)
	      (when answer
		(tree-set (focus-tree) i answer)))
	    "string"
	    (lambda ()
	      (list (tree->string (tree-ref t i))))
	    w)
      (list 'group "[n.a.]")))

(define (image-handles t)
  (list (list 'group "Width")
	(string-input-handle t 1 "3em")
	(list 'group "Height")
	(string-input-handle t 2 "3em")))

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
	       ;; FIXME: itemize, enumerate, eqnarray*
	       (list (list 'check "Numbered" "v"
			   (lambda () (check-number? (focus-tree))))
		     (lambda () (number-toggle (focus-tree)))))
	  (opt (toggle-context? t)
	       (list (list 'check "Unfolded" "v"
			   (lambda () (toggle-second-context? (focus-tree))))
		     (lambda () (toggle-toggle (focus-tree)))))
	  (opt #t
	       (list "Describe"
		     (lambda () (set-message "Not yet implemented" ""))))
	  (opt #t
	       (list "Delete"
		     (lambda () (remove-structure-upwards))))

	  (list '---)
	  (list (list "Previous similar"
		      (lambda () (traverse-previous))))
	  (list (list "Next similar"
		      (lambda () (traverse-next))))
	  (list (list "First similar"
		      (lambda () (traverse-first))))
	  (list (list "Last similar"
		      (lambda () (traverse-last))))
	  (list (list "Exit left"
		      (lambda () (structured-exit-left))))
	  (list (list "Exit right"
		      (lambda () (structured-exit-right))))

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

(define (vertical-separator)
  (list '|))

(tm-define (standard-focus-icons t)
  (append (opt #t
	       (cons* '=>
		      (list 'balloon (tag-menu-name (tree-label t))
			    "Structured variant")
		      (variant-menu-items t)))
	  (opt (numbered-context? t)
	       ;; FIXME: itemize, enumerate, eqnarray*
	       (list (list 'balloon (list 'icon "tm_numbered.xpm")
			   "Toggle numbering")
		     (lambda () (number-toggle (focus-tree)))))
	  (opt (toggle-context? t)
	       (list (list 'balloon (list 'icon "tm_unfold.xpm")
			   "Fold / Unfold")
		     (lambda () (toggle-toggle (focus-tree)))))
	  (opt #t
	       (list (list 'balloon (list 'icon "tm_focus_help.xpm")
			   "Describe tag")
		     (lambda () (set-message "Not yet implemented" ""))))
	  (opt #t
	       (list (list 'balloon (list 'icon "tm_focus_delete.xpm")
			   "Remove tag")
		     (lambda () (remove-structure-upwards))))

	  (vertical-separator)
	  (list (list (list 'balloon (list 'icon "tm_similar_first.xpm")
			    "Go to first similar tag")
		      (lambda () (traverse-first))))
	  (list (list (list 'balloon (list 'icon "tm_similar_previous.xpm")
			    "Go to previous similar tag")
		      (lambda () (traverse-previous))))
	  (list (list (list 'balloon (list 'icon "tm_exit_left.xpm")
			    "Exit tag on the left")
		      (lambda () (structured-exit-left))))
	  (list (list (list 'balloon (list 'icon "tm_exit_right.xpm")
			    "Exit tag on the right")
		      (lambda () (structured-exit-right))))
	  (list (list (list 'balloon (list 'icon "tm_similar_next.xpm")
			    "Go to next similar tag")
		      (lambda () (traverse-next))))
	  (list (list (list 'balloon (list 'icon "tm_similar_last.xpm")
			    "Go to last similar tag")
		      (lambda () (traverse-last))))

	  (opt (or (structured-horizontal? t) (structured-vertical? t))
	       (vertical-separator))
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
		     (lambda () (structured-remove-down))))

	  (opt (tree-is? t 'image)
	       (cons (vertical-separator)
		     (image-handles t)))))

(tm-define (texmacs-focus-icons)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-icons t))))
