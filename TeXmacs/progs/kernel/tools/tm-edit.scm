
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
    make-compound-arg
    ;; inserting inactive content
    make-inactive make-inactive-arg make-inactive-message
    make-inactive-compound-arg make-inactive-compound-args
    make-inactive-assign-arg make-inactive-assign-function
    make-inactive-assign-function-arg))

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

(tm-define (make-compound-arg s)
  (:type (string ->))
  (:synopsis "Insert an expanded macro with name @s.")
  (make-compound-arity s 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-inactive name nrargs)
  (:type (string int ->))
  (:synopsis "Insert the inactive primitive @name with @nrargs arguments.")
  (make-deactivated name nrargs name))

(tm-define (make-inactive-arg name arg nrargs)
  (:type (string string int ->))
  (:synopsis "Insert the inactive primitve @name with @nrargs arguments and"
	     "first argument @arg.")
  (make-deactivated-arg name nrargs name arg))

(tm-define (make-inactive-message name nrargs message)
  (:type (string int string ->))
  (:synopsis "Insert the inactive primitve @name with @nrargs arguments and"
	     "display @message in the footer.")
  (make-deactivated name nrargs message))

(tm-define (make-inactive-compound-arg s)
  (:type (string ->))
  (:synopsis "Make an inactive function application with name @s.")
  (if (and (tmp-use-apply?) (not (== s "cite")) (not (== s "nocite")))
      (make-inactive "apply" 1)
      (make-inactive "compound" 1))
  (insert-string s)
  (insert-argument)
  (set-message "Press <Return> to activate" s))

(tm-define (make-inactive-compound-args s n)
  (:type (string int ->))
  (:synopsis "Make an inactive function application with name @s of arity @n.")
  (if (and (tmp-use-apply?) (not (== s "cite")) (not (== s "nocite")))
      (make-inactive "apply" n)
      (make-inactive "compound" n))
  (insert-string s)
  (insert-argument)
  (set-message "Press <Return> to activate" s))

(tm-define (make-inactive-assign-arg s)
  (:type (string ->))
  (:synopsis "Make an inactive assignment for the variable @s.")
  (insert-object-go-to `(inactive (assign ,s "")) '(0 1 0))
  (set-message "return: activate" "assign"))

(tm-define (make-inactive-assign-function s)
  (:type (string ->))
  (:synopsis "Make an inactive function assignment for the variable @s.")
  (insert-object-go-to
   `(inactive (assign ,s (inactive (func ""))))
   '(0 1 0 0 0))
  (set-message "return (2x): activate" "assign#function"))

(tm-define (make-inactive-assign-function-arg s)
  (:type (string ->))
  (:synopsis "Make an inactive unary function assignment for the variable @s.")
  (insert-object-go-to
   `(inactive (assign ,s (inactive (func "s" ""))))
   '(0 1 0 1 0))
  (set-message "return (2x): activate" "assign#function"))
