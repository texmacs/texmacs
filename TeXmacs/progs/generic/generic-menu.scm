
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
;; Subroutines for hidden fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hidden-child? t i)
  (and (not (tree-accessible-child? t i))
       (!= (type->format (tree-child-type t i)) "n.a.")))

(define (hidden-children t)
  (with fun (lambda (i) (if (hidden-child? t i) (list (tree-ref t i)) (list)))
    (append-map fun (.. 0 (tree-arity t)))))

(define (tree-child-name* t i)
  (with s (tree-child-name t i)
    (cond ((!= s "") s)
          ((> (length (hidden-children t)) 1) "")
          ((== (tree-child-type t i) "regular") "")
          (else (tree-child-type t i)))))

(define (tree-child-long-name* t i)
  (with s (tree-child-long-name t i)
    (cond ((!= s "") s)
          ((> (length (hidden-children t)) 1) "")
          ((== (tree-child-type t i) "regular") "")
          (else (tree-child-type t i)))))

(define (type->format type)
  (cond ((== type "adhoc") "n.a.")
        ((== type "raw") "n.a.")
        ((== type "url") "smart-file")
        ((== type "graphical") "n.a.")
        ((== type "point") "n.a.")
        ((== type "obsolete") "n.a.")
        ((== type "unknown") "n.a.")
        ((== type "error") "n.a.")
        (else "string")))

(define (type->width type)
  (cond ((== type "boolean") "3em")
        ((== type "integer") "3em")
        ((== type "length") "3em")
        ((== type "numeric") "3em")
        ((== type "identifier") "8em")
        ((== type "duration") "3em")
        (else "1w")))

(tm-menu (string-input-icon t i)
  (let* ((name (tree-child-name* t i))
         (s (string-append (upcase-first name) ":"))
         (active? (tree-atomic? (tree-ref t i)))
	 (in (if active? (tree->string (tree-ref t i)) "n.a."))
         (type (tree-child-type t i))
         (fm (type->format type))
         (w (type->width type)))
    (assuming (== name "")
      (glue #f #f 5 0))
    (assuming (!= name "")
      (glue #f #f 3 0)
      (mini #t (group (eval s))))
    (when active?
      (input (when answer (tree-set (focus-tree) i answer)) fm
	     (list in) w))))

(tm-menu (string-input-menu t i)
  (let* ((name (tree-child-long-name* t i))
         (s `(concat "Set " ,name))
         (prompt (upcase-first name))
         (type (tree-child-type t i))
         (fm (type->format type)))
    (assuming (!= name "")
      (when (tree-atomic? (tree-ref t i))
        ((eval s)
         (interactive (lambda (x) (tree-set (focus-tree) i x))
           (list prompt fm (tree->string (tree-ref t i)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Focus menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: when recovering focus-tree,
;; double check that focus-tree still has the required form

(tm-menu (focus-variant-menu t)
  (-> (eval (tag-menu-name (tree-label t)))
      (dynamic (variant-menu-items t)))
  (assuming (numbered-context? t)
    ;; FIXME: itemize, enumerate, eqnarray*
    ((check "Numbered" "v" (check-number? (focus-tree)))
     (number-toggle (focus-tree))))
  (assuming (toggle-context? t)
    ((check "Unfolded" "v" (toggle-second-context? (focus-tree)))
     (toggle-toggle (focus-tree))))
  ("Describe" (set-message "Not yet implemented" ""))
  ("Delete" (remove-structure-upwards)))

(tm-menu (focus-move-menu t)
  ("Previous similar" (traverse-previous))
  ("Next similar" (traverse-next))
  ("First similar" (traverse-first))
  ("Last similar" (traverse-last))
  (assuming (not (tree-none-accessible? t))
    ("Exit left" (structured-exit-left))
    ("Exit right" (structured-exit-right))))

(tm-menu (focus-insert-menu t)
  (assuming (and (structured-horizontal? t) (not (structured-vertical? t)))
    (when (focus-can-insert)
      ("Insert argument before" (structured-insert #f))
      ("Insert argument after" (structured-insert #t)))
    (when (focus-can-remove)
      ("Remove argument before" (structured-remove #f))
      ("Remove argument after" (structured-remove #t))))
  (assuming (structured-vertical? t)
    ("Insert above" (structured-insert-up))
    ("Insert left" (structured-insert #f))
    ("Insert right" (structured-insert #t))
    ("Insert below" (structured-insert-down))
    ("Remove upwards" (structured-remove-up))
    ("Remove leftwards" (structured-remove #f))
    ("Remove rightwards" (structured-remove #t))
    ("Remove downwards" (structured-remove-down))))

(tm-menu (focus-extra-menu t))

(tm-menu (focus-hidden-menu t)
  (assuming (nnull? (hidden-children t))
    ---
    (for (i (.. 0 (tree-arity t)))
      (assuming (hidden-child? t i)
        (dynamic (string-input-menu t i))))))

(tm-menu (standard-focus-menu t)
  (dynamic (focus-variant-menu t))
  ---
  (dynamic (focus-move-menu t))
  (assuming (or (structured-horizontal? t) (structured-vertical? t))
    ---
    (dynamic (focus-insert-menu t)))
  (dynamic (focus-extra-menu t))
  (dynamic (focus-hidden-menu t)))

(tm-menu (focus-menu)
  (dynamic (standard-focus-menu (focus-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main focus icons bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-variant-icons t)
  (mini #t
    (=> (balloon (eval (tag-menu-name (tree-label t))) "Structured variant")
	(dynamic (variant-menu-items t))))
  (assuming (numbered-context? t)
    ;; FIXME: itemize, enumerate, eqnarray*
    ((check (balloon (icon "tm_numbered.xpm") "Toggle numbering") "v"
	    (check-number? (focus-tree)))
     (number-toggle (focus-tree))))
  (assuming (toggle-context? t)
    ((check (balloon (icon "tm_unfold.xpm") "Fold / Unfold") "v"
	    (toggle-second-context? (focus-tree)))
     (toggle-toggle (focus-tree))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (set-message "Not yet implemented" ""))
  ((balloon (icon "tm_focus_delete.xpm") "Remove tag")
   (remove-structure-upwards)))

(tm-menu (focus-move-icons t)
  ((balloon (icon "tm_similar_first.xpm") "Go to first similar tag")
   (traverse-first))
  ((balloon (icon "tm_similar_previous.xpm") "Go to previous similar tag")
   (traverse-previous))
  (assuming (not (tree-none-accessible? t))
    ((balloon (icon "tm_exit_left.xpm") "Exit tag on the left")
     (structured-exit-left))
    ((balloon (icon "tm_exit_right.xpm") "Exit tag on the right")
     (structured-exit-right)))
  ((balloon (icon "tm_similar_next.xpm") "Go to next similar tag")
   (traverse-next))
  ((balloon (icon "tm_similar_last.xpm") "Go to last similar tag")
   (traverse-last)))

(tm-menu (focus-insert-icons t)
  (assuming (and (structured-horizontal? t) (not (structured-vertical? t)))
    (when (focus-can-insert)
      ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
       (structured-insert #f))
      ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
       (structured-insert #t)))
    (when (focus-can-remove)
      ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
       (structured-remove #f))
      ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
       (structured-remove #t))))
  (assuming (structured-vertical? t)
    ((balloon (icon "tm_insert_up.xpm") "Structured insert above")
     (structured-insert-up))
    ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
     (structured-insert #f))
    ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
     (structured-insert #t))
    ((balloon (icon "tm_insert_down.xpm") "Structured insert below")
     (structured-insert-down))
    ((balloon (icon "tm_delete_up.xpm") "Structured remove upwards")
     (structured-remove-up))
    ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
     (structured-remove #f))
    ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
     (structured-remove #t))
    ((balloon (icon "tm_delete_down.xpm") "Structured remove downwards")
     (structured-remove-down))))

(tm-menu (focus-extra-icons t))

(tm-menu (focus-hidden-icons t)
  (for (i (.. 0 (tree-arity t)))
    (assuming (hidden-child? t i)
      (dynamic (string-input-icon t i)))))

(tm-menu (standard-focus-icons t)
  (minibar (dynamic (focus-variant-icons t)))
  (glue #f #f 5 0)
  (minibar (dynamic (focus-move-icons t)))
  (assuming (or (structured-horizontal? t) (structured-vertical? t))
    (glue #f #f 5 0)
    (minibar (dynamic (focus-insert-icons t))))
  (dynamic (focus-extra-icons t))
  (dynamic (focus-hidden-icons t))
  (glue #f #f 5 0))

(tm-menu (texmacs-focus-icons)
  (dynamic (standard-focus-icons (focus-tree))))
