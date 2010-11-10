
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

(tm-define-macro (gui$cond pred? . l)
  (:synopsis "Conditionally make widgets")
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

(tm-define-macro (gui$group text)
  (:synopsis "Make a menu group")
  `(list 'group ,text))

(tm-define-macro (gui$input cmd type proposals width)
  (:synopsis "Make input field")
  `(list 'input (lambda (answer) ,cmd) ,type (lambda () ,proposals) ,width))

(tm-define-macro (gui$button text . cmds)
  (:synopsis "Make button")
  `(list ,text (lambda () ,@cmds)))

(tm-define-macro (gui$pullright text . l)
  (:synopsis "Make pullright button")
  `(cons* '-> ,text (gui$list ,@l)))

(tm-define-macro (gui$pulldown text . l)
  (:synopsis "Make pulldown button")
  `(cons* '=> ,text (gui$list ,@l)))

(tm-define-macro (gui$minibar . l)
  (:synopsis "Make minibar")
  `(cons* 'minibar (gui$list ,@l)))

(tm-define-macro (gui$check text check pred?)
  (:synopsis "Make button")
  `(list 'check ,text ,check (lambda () ,pred?)))

(tm-define-macro (gui$mini pred? . l)
  (:synopsis "Make mini widgets")
  `(cons* 'mini (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$hsep)
  (:synopsis "Make horizontal separator")
  `(string->symbol "|"))

(tm-define-macro (gui$vsep)
  (:synopsis "Make vertical separator")
  `'---)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (require-format x pattern)
  (if (not (match? x pattern))
      (texmacs-error "gui-menu-item" "invalid menu item ~S" x)))

(tm-define (gui-menu-item x)
  (:case eval)
  (require-format x '(eval :%1))
  (cadr x))

(tm-define (gui-menu-item x)
  (:case dynamic)
  (require-format x '(dynamic :%1))
  `(gui$dynamic ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case group)
  (require-format x '(group :string?))
  `(gui$group ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case icon)
  (require-format x '(icon :string?))
  `(gui$icon ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case check)
  (require-format x '(check :%1 :string? :%1))
  `(gui$check ,(gui-menu-item (cadr x)) ,(caddr x) ,(cadddr x)))

(tm-define (gui-menu-item x)
  (:case balloon)
  (require-format x '(balloon :%1 :string?))
  `(gui$balloon ,(gui-menu-item (cadr x)) ,(caddr x)))

(tm-define (gui-menu-item x)
  (:case ->)
  (require-format x '(-> :%1 :*))
  `(gui$pullright ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case =>)
  (require-format x '(=> :%1 :*))
  `(gui$pullright ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case cond)
  (require-format x '(cond :%1 :*))
  `(gui$cond ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case when)
  (require-format x '(when :%1 :*))
  `(gui$when ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (cond ((== x '---) `(gui$vsep))
	((== x (string->symbol "|")) `(gui$hsep))
	((string? x) x)
	((and (pair? x) (or (string? (car x)) (pair? (car x))))
	 `(gui$button ,(gui-menu-item (car x)) ,@(cdr x)))
        (else (texmacs-error "gui-menu-item" "invalid menu item ~S" x))))

(tm-define-macro (gui$item x)
  (gui-menu-item x))

(tm-define-macro (gui$menu . l)
  `(gui$list ,@(map gui-menu-item l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special handles for images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-input-handle t i w)
  (if (tree-atomic? (tree-ref t i))
      (gui$input (when answer (tree-set (focus-tree) i answer)) "string"
		 (list (tree->string (tree-ref t i))) w)
      (gui$item (group "[n.a.]"))))

(define (image-handles t)
  (list (gui$mini #t (gui$item (group "Width:")))
	(string-input-handle t 1 "3em")
	(gui$mini #t (gui$item (group "Height:")))
	(string-input-handle t 2 "3em")
	(gui$mini #t (gui$item (group "File:")))
	(string-input-handle t 0 "0.5w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Focus menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: when recovering focus-tree,
;; double check that focus-tree still has the required form

(tm-define (standard-focus-menu t)
  (gui$list
    (gui$pullright (tag-menu-name (tree-label t))
		   (gui$item (dynamic (variant-menu-items t))))
    (gui$item (cond (numbered-context? t)
		    ;; FIXME: itemize, enumerate, eqnarray*
		    ((check "Numbered" "v"
			    (check-number? (focus-tree)))
		     (number-toggle (focus-tree)))))
    (gui$item (cond (toggle-context? t)
		    ((check "Unfolded" "v"
			    (toggle-second-context? (focus-tree)))
		     (toggle-toggle (focus-tree)))))
    (gui$item ("Describe" (set-message "Not yet implemented" "")))
    (gui$item ("Delete" (remove-structure-upwards)))

    (gui$item ---)
    (gui$item ("Previous similar" (traverse-previous)))
    (gui$item ("Next similar" (traverse-next)))
    (gui$item ("First similar" (traverse-first)))
    (gui$item ("Last similar" (traverse-last)))
    (gui$item ("Exit left" (structured-exit-left)))
    (gui$item ("Exit right" (structured-exit-right)))

    (gui$item (cond (or (structured-horizontal? t) (structured-vertical? t))
		    ---))
    (gui$item (cond (structured-vertical? t)
		    ("Insert above" (structured-insert-up))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-insert)
		      ("Insert argument before"
		       (structured-insert #f)))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-insert)
		      ("Insert argument after"
		       (structured-insert #t)))))
    (gui$item (cond (structured-vertical? t)
		    ("Insert below" (structured-insert-down))))
    (gui$item (cond (structured-vertical? t)
		    ("Remove upwards" (structured-remove-up))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-remove)
		      ("Remove argument before"
		       (structured-remove #f)))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-remove)
		      ("Remove argument after"
		       (structured-remove #t)))))
    (gui$item (cond (structured-vertical? t)
		    ("Remove downwards"
		     (structured-remove-down))))))

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
              (gui$pulldown (gui$item
			     (balloon (eval (tag-menu-name (tree-label t)))
				      "Structured variant"))
			    (gui$item (dynamic (variant-menu-items t)))))
    (gui$item (cond (numbered-context? t)
		    ;; FIXME: itemize, enumerate, eqnarray*
		    ((check (balloon (icon "tm_numbered.xpm")
				     "Toggle numbering")
			    "v"
			    (check-number? (focus-tree)))
		     (number-toggle (focus-tree)))))
    (gui$item (cond (toggle-context? t)
		    ((check (balloon (icon "tm_unfold.xpm")
				     "Fold / Unfold")
			    "v"
			    (toggle-second-context? (focus-tree)))
		     (toggle-toggle (focus-tree)))))
    (gui$item ((balloon (icon "tm_focus_help.xpm")
			"Describe tag")
	       (set-message "Not yet implemented" "")))
    (gui$item ((balloon (icon "tm_focus_delete.xpm")
			"Remove tag")
	       (remove-structure-upwards)))))

(tm-define (focus-move-icons t)
  (gui$list
    (gui$item ((balloon (icon "tm_similar_first.xpm")
		       "Go to first similar tag")
	       (traverse-first)))
    (gui$item ((balloon (icon "tm_similar_previous.xpm")
			"Go to previous similar tag")
	       (traverse-previous)))
    (gui$item ((balloon (icon "tm_exit_left.xpm")
			"Exit tag on the left")
	       (structured-exit-left)))
    (gui$item ((balloon (icon "tm_exit_right.xpm")
			"Exit tag on the right")
	       (structured-exit-right)))
    (gui$item ((balloon (icon "tm_similar_next.xpm")
			"Go to next similar tag")
	       (traverse-next)))
    (gui$item ((balloon (icon "tm_similar_last.xpm")
			"Go to last similar tag")
	       (traverse-last)))))

(tm-define (focus-insert-icons t)
  (gui$list
    (gui$item (cond (structured-vertical? t)
		    ((balloon (icon "tm_insert_up.xpm")
			      "Structured insert above")
		     (structured-insert-up))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-insert)
		      ((balloon (icon "tm_insert_left.xpm")
				"Structured insert at the left")
		       (structured-insert #f)))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-insert)
		      ((balloon (icon "tm_insert_right.xpm")
				"Structured insert at the right")
		       (structured-insert #t)))))
    (gui$item (cond (structured-vertical? t)
		    ((balloon (icon "tm_insert_down.xpm")
			      "Structured insert below")
		     (structured-insert-down))))
    (gui$item (cond (structured-vertical? t)
		    ((balloon (icon "tm_delete_up.xpm")
			      "Structured remove upwards")
		     (structured-remove-up))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-remove)
		      ((balloon (icon "tm_delete_left.xpm")
				"Structured remove leftwards")
		       (structured-remove #f)))))
    (gui$item (cond (structured-horizontal? t)
		    (when (focus-can-remove)
		      ((balloon (icon "tm_delete_right.xpm")
				"Structured remove rightwards")
		       (structured-remove #t)))))
    (gui$item (cond (structured-vertical? t)
		    ((balloon (icon "tm_delete_down.xpm")
			      "Structured remove downwards")
		     (structured-remove-down))))))

(tm-define (standard-focus-icons t)
  (gui$list
    (gui$minibar (gui$item (dynamic (focus-variant-icons t))))
    (gui$item (group ""))
    (gui$minibar (gui$item (dynamic (focus-move-icons t))))
    (gui$cond (or (structured-horizontal? t) (structured-vertical? t))
      (gui$item (group ""))
      (gui$minibar (gui$item (dynamic (focus-insert-icons t)))))
    (gui$item (cond (tree-is? t 'image)
		    (group "")
		    (dynamic (image-handles t))
		    (group "")))))

(tm-define (texmacs-focus-icons)
  (with t (focus-tree)
    (menu-dynamic
      ,@(standard-focus-icons t))))
