
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
	(generic format-geometry-edit)
	(generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-variants-of t)
  (variants-of (tree-label t)))

(tm-define (focus-tag-name l)
  (if (symbol-unnumbered? l)
      (focus-tag-name (symbol-drop-right l 1))
      (upcase-first (tree-name (tree l)))))

(tm-menu (focus-variant-menu t)
  (for (v (focus-variants-of t))
    ((eval (focus-tag-name v))
     (variant-set-keep-numbering (focus-tree) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding and removing children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  #t)

(tm-define (focus-can-insert-remove? t)
  (and (or (structured-horizontal? t) (structured-vertical? t))
       (cursor-inside? t)))

(tm-define (focus-can-insert?)
  (with t (focus-tree)
    (< (tree-arity t) (tree-maximal-arity t))))

(tm-define (focus-can-remove?)
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

(tm-menu (focus-ancestor-menu t))

(tm-menu (focus-toggle-menu t)
  (assuming (numbered-context? t)
    ;; FIXME: itemize, enumerate, eqnarray*
    ((check "Numbered" "v" (numbered-numbered? (focus-tree)))
     (numbered-toggle (focus-tree))))
  (assuming (alternate-context? t)
    ((check (eval (alternate-second-name t)) "v"
            (alternate-second? (focus-tree)))
     (alternate-toggle (focus-tree)))))

(tm-menu (focus-tag-menu t)
  (with l (focus-variants-of t)
    (assuming (<= (length l) 1)
      (inert ((eval (focus-tag-name (tree-label t))) (noop) (noop))))
    (assuming (> (length l) 1)
      (-> (eval (focus-tag-name (tree-label t)))
          (dynamic (focus-variant-menu t)))))
  (dynamic (focus-toggle-menu t))
  ("Describe" (set-message "Not yet implemented" ""))
  ("Delete" (remove-structure-upwards)))

(tm-menu (focus-move-menu t)
  ("Previous similar" (traverse-previous))
  ("Next similar" (traverse-next))
  ("First similar" (traverse-first))
  ("Last similar" (traverse-last))
  (assuming (cursor-inside? t)
    ("Exit left" (structured-exit-left))
    ("Exit right" (structured-exit-right))))

(tm-menu (focus-insert-menu t)
  (assuming (and (structured-horizontal? t) (not (structured-vertical? t)))
    (when (focus-can-insert?)
      ("Insert argument before" (structured-insert-left))
      ("Insert argument after" (structured-insert-right)))
    (when (focus-can-remove?)
      ("Remove argument before" (structured-remove-left))
      ("Remove argument after" (structured-remove-right))))
  (assuming (structured-vertical? t)
    ("Insert above" (structured-insert-up))
    ("Insert left" (structured-insert-left))
    ("Insert right" (structured-insert-right))
    ("Insert below" (structured-insert-down))
    ("Remove upwards" (structured-remove-up))
    ("Remove leftwards" (structured-remove-left))
    ("Remove rightwards" (structured-remove-right))
    ("Remove downwards" (structured-remove-down))))

(tm-menu (focus-extra-menu t))

(tm-define (hidden-string-children t)
  (append-map (lambda (c) (if (tree-atomic? c) (list c) (list)))
              (hidden-children t)))

(tm-menu (focus-hidden-menu t)
  (assuming (nnull? (hidden-string-children t))
    ---
    (for (i (.. 0 (tree-arity t)))
      (assuming (hidden-child? t i)
        (dynamic (string-input-menu t i))))))

(tm-menu (focus-hidden-menu t)
  (:require (alternate-context? t)))

(tm-menu (standard-focus-menu t)
  (dynamic (focus-ancestor-menu t))
  (dynamic (focus-tag-menu t))
  (assuming (focus-can-move? t)
    ---
    (dynamic (focus-move-menu t)))
  (assuming (focus-can-insert-remove? t)
    ---
    (dynamic (focus-insert-menu t)))
  (dynamic (focus-extra-menu t))
  (dynamic (focus-hidden-menu t)))

(tm-menu (focus-menu)
  (dynamic (standard-focus-menu (focus-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main focus icons bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-ancestor-icons t))

(tm-menu (focus-toggle-icons t)
  (assuming (numbered-context? t)
    ((check (balloon (icon "tm_numbered.xpm") "Toggle numbering") "v"
            (numbered-numbered? (focus-tree)))
     (numbered-toggle (focus-tree))))
  (assuming (alternate-first? t)
    ((check (balloon (icon "tm_alternate_first.xpm")
                     (eval (alternate-second-name t))) "v" #f)
     (alternate-toggle (focus-tree))))
  (assuming (alternate-second? t)
    ((check (balloon (icon (eval (alternate-second-icon t)))
                     (eval (alternate-second-name t))) "v" #t)
     (alternate-toggle (focus-tree)))))

(tm-menu (focus-tag-icons t)
  (dynamic (focus-toggle-icons t))
  (mini #t
    (with l (focus-variants-of t)
      (assuming (<= (length l) 1)
        (inert ((eval (focus-tag-name (tree-label t))) (noop))))
      (assuming (> (length l) 1)
        (=> (balloon (eval (focus-tag-name (tree-label t)))
                     "Structured variant")
            (dynamic (focus-variant-menu t))))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (set-message "Not yet implemented" ""))
  ((balloon (icon "tm_focus_delete.xpm") "Remove tag")
   (remove-structure-upwards)))

(tm-menu (focus-move-icons t)
  ((balloon (icon "tm_similar_first.xpm") "Go to first similar tag")
   (traverse-first))
  ((balloon (icon "tm_similar_previous.xpm") "Go to previous similar tag")
   (traverse-previous))
  (assuming (cursor-inside? t)
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
    (when (focus-can-insert?)
      ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
       (structured-insert-left))
      ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
       (structured-insert-right)))
    (when (focus-can-remove?)
      ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
       (structured-remove-left))
      ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
       (structured-remove-right))))
  (assuming (structured-vertical? t)
    ((balloon (icon "tm_insert_up.xpm") "Structured insert above")
     (structured-insert-up))
    ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
     (structured-insert-left))
    ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
     (structured-insert-right))
    ((balloon (icon "tm_insert_down.xpm") "Structured insert below")
     (structured-insert-down))
    ((balloon (icon "tm_delete_up.xpm") "Structured remove upwards")
     (structured-remove-up))
    ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
     (structured-remove-left))
    ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
     (structured-remove-right))
    ((balloon (icon "tm_delete_down.xpm") "Structured remove downwards")
     (structured-remove-down))))

(tm-menu (focus-extra-icons t))

(tm-menu (focus-hidden-icons t)
  (for (i (.. 0 (tree-arity t)))
    (assuming (hidden-child? t i)
      (dynamic (string-input-icon t i)))))

(tm-menu (focus-hidden-icons t)
  (:require (alternate-context? t)))

(tm-menu (standard-focus-icons t)
  (dynamic (focus-ancestor-icons t))
  (assuming (focus-can-move? t)
    (minibar (dynamic (focus-move-icons t)))
    (glue #f #f 5 0))
  (assuming (focus-can-insert-remove? t)
    (minibar (dynamic (focus-insert-icons t)))
    (glue #f #f 5 0))
  (minibar (dynamic (focus-tag-icons t)))
  (dynamic (focus-extra-icons t))
  (dynamic (focus-hidden-icons t))
  (glue #f #f 5 0))

(tm-menu (texmacs-focus-icons)
  (assuming (in-graphics?)
    (dynamic (graphics-focus-icons)))
  (assuming (not (in-graphics?))
    (dynamic (standard-focus-icons (focus-tree)))))
