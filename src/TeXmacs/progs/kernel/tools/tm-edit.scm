
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-edit.scm
;; DESCRIPTION : important subroutines for editing
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel tools tm-edit)
  (:use (kernel texmacs tm-define))
  (:export
    ;; inserting general content
    insert-object insert-object-go-to
    insert-tree-at insert-object-at
    ;; inserting inactive content
    make-assign-arg make-assign-macro make-assign-macro-arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-object obj)
  (:type (object ->))
  (:synopsis "Insert @obj at the current cursor position.")
  (insert-tree (object->tree obj)))

(tm-define (insert-object-go-to obj p)
  (:type (object path ->))
  (:synopsis "Insert @obj and move cursor to @p inside @obj.")
  (insert-tree-go-to (object->tree obj) p))

(tm-define (insert-tree-at t p)
  (:type (tree path ->))
  (:synopsis "Insert @t at @p.")
  (let* ((pos (tm-position-new))
	 (old (tm-position-get pos)))
    (tm-go-to p)
    (insert-tree t)
    (tm-go-to old)
    (tm-position-delete pos)))

(tm-define (insert-object-at obj p)
  (:type (object path ->))
  (:synopsis "Insert @obj at @p.")
  (insert-tree-at (object->tree obj) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-inactive-object-go-to t p)
  (:type (object path ->))
  (:synopsis "Insert an inactive object @t and go to @p inside @t.")
  (if (in-preamble?)
      (insert-object-go-to t p)
      (insert-object-go-to (list 'inactive t) (cons 0 p))))

(tm-define (make-assign-arg s)
  (:type (string ->))
  (:synopsis "Make an inactive assignment for the variable @s.")
  (insert-inactive-object-go-to `(assign ,s "") '(1 0))
  (if (not (in-preamble?)) (set-message "return: activate" "assign")))

(tm-define (make-assign-macro s)
  (:type (string ->))
  (:synopsis "Make an inactive macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-object-go-to '(macro "") '(0 0))
  (if (not (in-preamble?))
      (set-message "return (2x): activate" "assign#macro")))

(tm-define (make-assign-macro-arg s)
  (:type (string ->))
  (:synopsis "Make an inactive unary macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-object-go-to '(macro "s" "") '(1 0))
  (if (not (in-preamble?))
      (set-message "return (2x): activate" "assign#macro")))
