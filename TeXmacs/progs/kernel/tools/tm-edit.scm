
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
    in-preamble?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (in-preamble?)
  (:type (-> bool))
  (:synopsis "Are we in preamble mode?")
  (string=? (get-env "preamble") "true"))

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
