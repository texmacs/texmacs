
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
    insert-stree insert-stree-go-to
    insert-tree-at insert-stree-at
    ;; inserting inactive content
    make-assign-arg make-assign-macro make-assign-macro-arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-stree t)
  (:type (-> stree void))
  (:synopsis "Insert @t at the current cursor position.")
  (insert-tree (stree->tree t)))

(tm-define (insert-stree-go-to t p)
  (:type (-> stree path void))
  (:synopsis "Insert @t and move cursor to @p inside @t.")
  (insert-tree-go-to (stree->tree t) p))

(tm-define (insert-tree-at t p)
  (:type (-> tree path void))
  (:synopsis "Insert @t at @p.")
  (let* ((pos (tm-position-new))
	 (old (tm-position-get pos)))
    (tm-go-to p)
    (insert-tree t)
    (tm-go-to old)
    (tm-position-delete pos)))

(tm-define (insert-stree-at t p)
  (:type (-> stree path void))
  (:synopsis "Insert @t at @p.")
  (insert-tree-at (stree->tree t) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-inactive-stree-go-to t p)
  (:type (-> stree path void))
  (:synopsis "Insert an inactive stree @t and go to @p inside @t.")
  (if (in-source?)
      (insert-stree-go-to t p)
      (insert-stree-go-to (list 'inactive t) (cons 0 p))))

(tm-define (make-assign-arg s)
  (:type (-> string void))
  (:synopsis "Make an inactive assignment for the variable @s.")
  (insert-inactive-stree-go-to `(assign ,s "") '(1 0))
  (if (not (in-source?)) (set-message "return: activate" "assign")))

(tm-define (make-assign-macro s)
  (:type (-> string void))
  (:synopsis "Make an inactive macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-stree-go-to '(macro "") '(0 0))
  (if (not (in-source?))
      (set-message "return (2x): activate" "assign#macro")))

(tm-define (make-assign-macro-arg s)
  (:type (-> string void))
  (:synopsis "Make an inactive unary macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-stree-go-to '(macro "s" "") '(1 0))
  (if (not (in-source?))
      (set-message "return (2x): activate" "assign#macro")))
