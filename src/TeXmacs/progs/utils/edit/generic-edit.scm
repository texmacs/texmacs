
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-edit.scm
;; DESCRIPTION : important subroutines for editing
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit generic-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-at t p)
  (:type (-> tree path void))
  (:synopsis "Insert @t at @p.")
  (let* ((pos (tm-position-new))
	 (old (tm-position-get pos)))
    (tm-go-to p)
    (insert t)
    (tm-go-to old)
    (tm-position-delete pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-inactive-stree-go-to t p)
  (:type (-> stree path void))
  (:synopsis "Insert an inactive stree @t and go to @p inside @t.")
  (if (in-source?)
      (insert-go-to t p)
      (insert-go-to (list 'inactive t) (cons 0 p))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-specific s)
  (if (or (== s "texmacs") (in-source?))
      (insert-go-to `(specific ,s "") '(1 0))
      (insert-go-to `(inactive (specific ,s "")) '(0 1 0))))

(tm-define (make-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))

(tm-define (make-include u)
  (insert `(include ,(string-slash (url->string u)))))

(tm-define (make-inline-image l)
  (apply make-postscript (cons* (url->string (car l)) #f (cdr l))))

(tm-define (make-link-image l)
  (apply make-postscript (cons* (url->string (car l)) #t (cdr l))))
