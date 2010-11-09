
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
;; New markup element for generation of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-normalize l)
  (cond ((null? l) l)
	((func? (car l) 'list)
	 (append (gui-normalize (cdar l)) (gui-normalize (cdr l))))
	(else (cons (car l) (gui-normalize (cdr l))))))

(tm-define-macro (gui$list . l)
  (:synopsis "Make widgets")
  `(gui-normalize (list ,@l)))

(tm-define-macro (gui$dynamic w)
  (:synopsis "Make dynamic widgets")
  `(cons* 'list ,w))

(tm-define-macro (gui$optional pred? . l)
  (:synopsis "Make optional widgets")
  `(cons* 'list (if ,pred? (gui$list ,@l) '())))

(tm-define-macro (gui$when pred? . l)
  (:synopsis "Make possibly inert (whence greyed) widgets")
  `(cons* 'when (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$icon name)
  (:synopsis "Make icon")
  `(list 'icon ,name))

(tm-define-macro (gui$balloon text balloon)
  (:synopsis "Make balloon")
  `(list 'balloon ,text ,balloon))

(tm-define-macro (gui$button text cmd)
  (:synopsis "Make button")
  `(list ,text (lambda () ,cmd)))

(tm-define-macro (gui$pullright text . l)
  (:synopsis "Make pullright button")
  `(cons* '-> ,text (gui$list ,@l)))

(tm-define-macro (gui$pulldown text . l)
  (:synopsis "Make pulldown button")
  `(cons* '=> ,text (gui$list ,@l)))

(tm-define-macro (gui$check text check pred?)
  (:synopsis "Make button")
  `(list 'check ,text ,check (lambda () ,pred?)))

(tm-define-macro (gui$mini pred? . l)
  (:synopsis "Make minibar")
  `(cons* 'mini (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$hsep)
  (:synopsis "Make horizontal separator")
  `(string->symbol "|"))

(tm-define-macro (gui$vsep)
  (:synopsis "Make vertical separator")
  `'---)

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
  (list (list 'mini (lambda () #t) (list 'group "Width:"))
	(string-input-handle t 1 "3em")
	(list 'mini (lambda () #t) (list 'group "Height:"))
	(string-input-handle t 2 "3em")
	(list 'mini (lambda () #t) (list 'group "File:"))
	(string-input-handle t 0 "0.5w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Focus menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (opt cond? . l)
  `(if ,cond? (list ,@l) '()))

;; FIXME: when recovering focus-tree,
;; double check that focus-tree still has the required form

(tm-define (standard-focus-menu t)
  (gui$list
    (gui$pullright (tag-menu-name (tree-label t))
		   (gui$dynamic (variant-menu-items t)))
    (gui$optional (numbered-context? t)
		  ;; FIXME: itemize, enumerate, eqnarray*
		  (gui$button
		   (gui$check "Numbered" "v"
			      (check-number? (focus-tree)))
		   (number-toggle (focus-tree))))
    (gui$optional (toggle-context? t)
		  (gui$button
		   (gui$check "Unfolded" "v"
			      (toggle-second-context? (focus-tree)))
		   (toggle-toggle (focus-tree))))
    (gui$button "Describe" (set-message "Not yet implemented" ""))
    (gui$button "Delete" (remove-structure-upwards))

    (gui$vsep)
    (gui$button "Previous similar" (traverse-previous))
    (gui$button "Next similar" (traverse-next))
    (gui$button "First similar" (traverse-first))
    (gui$button "Last similar" (traverse-last))
    (gui$button "Exit left" (structured-exit-left))
    (gui$button "Exit right" (structured-exit-right))

    (gui$optional (or (structured-horizontal? t) (structured-vertical? t))
		  (gui$vsep))
    (gui$optional (structured-vertical? t)
		  (gui$button "Insert above" (structured-insert-up)))
    (gui$optional (structured-horizontal? t)
		  (gui$when (focus-can-insert)
			    (gui$button "Insert argument before"
					(structured-insert #f))))
    (gui$optional (structured-horizontal? t)
		  (gui$when (focus-can-insert)
			    (gui$button "Insert argument after"
					(structured-insert #t))))
    (gui$optional (structured-vertical? t)
		  (gui$button "Insert below" (structured-insert-down)))
    (gui$optional (structured-vertical? t)
		  (gui$button "Remove upwards" (structured-remove-up)))
    (gui$optional (structured-horizontal? t)
		  (gui$when (focus-can-remove)
			    (gui$button "Remove argument before"
					(structured-remove #f))))
    (gui$optional (structured-horizontal? t)
		  (gui$when (focus-can-remove)
			    (gui$button "Remove argument after"
					(structured-remove #t))))
    (gui$optional (structured-vertical? t)
		  (gui$button "Remove downwards"
			      (structured-remove-down)))))

(tm-define (focus-menu)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-menu t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main focus icons bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-variant-icons t)
  (gui$list
    (gui$mini #t
              (gui$pulldown (gui$balloon (tag-menu-name (tree-label t))
					 "Structured variant")
			    (gui$dynamic (variant-menu-items t))))
    (gui$optional (numbered-context? t)
      ;; FIXME: itemize, enumerate, eqnarray*
      (gui$button (gui$check (gui$balloon (gui$icon "tm_numbered.xpm")
					  "Toggle numbering")
			     "v"
			     (check-number? (focus-tree)))
		  (number-toggle (focus-tree))))
    (gui$optional (toggle-context? t)
      (gui$button (gui$check (gui$balloon (gui$icon "tm_unfold.xpm")
					  "Fold / Unfold")
			     "v"
			     (toggle-second-context? (focus-tree)))
		  (toggle-toggle (focus-tree))))
    (gui$button (gui$balloon (gui$icon "tm_focus_help.xpm")
			     "Describe tag")
		(set-message "Not yet implemented" ""))
    (gui$button (gui$balloon (gui$icon "tm_focus_delete.xpm")
			     "Remove tag")
		(remove-structure-upwards))))

(tm-define (focus-move-icons t)
  (gui$list
    (gui$button (gui$balloon (gui$icon "tm_similar_first.xpm")
			     "Go to first similar tag")
		(traverse-first))
    (gui$button (gui$balloon (gui$icon "tm_similar_previous.xpm")
			     "Go to previous similar tag")
		(traverse-previous))
    (gui$button (gui$balloon (gui$icon "tm_exit_left.xpm")
			     "Exit tag on the left")
		(structured-exit-left))
    (gui$button (gui$balloon (gui$icon "tm_exit_right.xpm")
			     "Exit tag on the right")
		(structured-exit-right))
    (gui$button (gui$balloon (gui$icon "tm_similar_next.xpm")
			     "Go to next similar tag")
		(traverse-next))
    (gui$button (gui$balloon (gui$icon "tm_similar_last.xpm")
			     "Go to last similar tag")
		(traverse-last))))

(tm-define (focus-insert-icons t)
  (gui$list
    (gui$optional (structured-vertical? t)
      (gui$button (gui$balloon (gui$icon "tm_insert_up.xpm")
			       "Structured insert above")
		  (structured-insert-up)))
    (gui$optional (structured-horizontal? t)
      (gui$when (focus-can-insert)
		(gui$button (gui$balloon (gui$icon "tm_insert_left.xpm")
					 "Structured insert at the left")
			    (structured-insert #f))))
    (gui$optional (structured-horizontal? t)
      (gui$when (focus-can-insert)
		(gui$button (gui$balloon (gui$icon "tm_insert_right.xpm")
					 "Structured insert at the right")
			    (structured-insert #t))))
    (gui$optional (structured-vertical? t)
      (gui$button (gui$balloon (gui$icon "tm_insert_down.xpm")
			       "Structured insert below")
		  (structured-insert-down)))
    (gui$optional (structured-vertical? t)
      (gui$button (gui$balloon (gui$icon "tm_delete_up.xpm")
			       "Structured remove upwards")
		  (structured-remove-up)))
    (gui$optional (structured-horizontal? t)
      (gui$when (focus-can-remove)
		(gui$button (gui$balloon (gui$icon "tm_delete_left.xpm")
					 "Structured remove leftwards")
			    (structured-remove #f))))
    (gui$optional (structured-horizontal? t)
      (gui$when (focus-can-remove)
		(gui$button (gui$balloon (gui$icon "tm_delete_right.xpm")
					 "Structured remove rightwards")
			    (structured-remove #t))))
    (gui$optional (structured-vertical? t)
      (gui$button (gui$balloon (gui$icon "tm_delete_down.xpm")
			       "Structured remove downwards")
		  (structured-remove-down)))))

(tm-define (standard-focus-icons t)
  (append (opt #t
	       (cons 'minibar (focus-variant-icons t)))

	  (opt #t
	       (list 'group "")
	       (cons 'minibar (focus-move-icons t)))

          (opt (or (structured-horizontal? t) (structured-vertical? t))
	       (list 'group "")
	       (cons 'minibar (focus-insert-icons t)))

	  (opt (tree-is? t 'image)
	       (append (list (list 'group ""))
		       (image-handles t)
		       (list (list 'group ""))))))

(tm-define (texmacs-focus-icons)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-icons t))))
