
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-bind.scm
;; DESCRIPTION : binding variables to values
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-bind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Is an expression a free variable?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (free-variable name)
  (list 'quote name))

(define-public (free-variable? expr)
  "Is the expression @expr a free variable?"
  (and (list? expr) (= (length expr) 2) (== (car expr) 'quote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binding variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (bind-var var val bl)
  "Force a binding of variable @var to @val in @bl."
  (list (acons var val bl)))

(define-public (bind-unify var val bl)
  "Bind variable @var to @val in @bl and unify if the binding already exists."
  (if (free-variable? val)
      (let* ((var2 (cadr val))
	     (val2 (assoc-ref bl var2)))
	(if val2 (bind-unify var val2 bl)
	    (let ((old-val (assoc-ref bl var)))
	      (cond (old-val (bind-unify var2 old-val bl))
		    ((== var var2) (list bl))
		    ((and (number? var2)
			  (or (not (number? var)) (< var var2)))
		     (bind-var var2 (free-variable var) bl))
		    (else (bind-var var val bl))))))
      (let ((old-val (assoc-ref bl var)))
	(cond ((not old-val) (bind-var var val bl))
	      ((free-variable? old-val)
	       (bind-unify (cadr old-val) val bl))
	      (else (unify (list val) (list old-val) bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (bind-substitute expr bl)
  "Substitute bindings @bl in expression @expr."
  ;; does full substitution if the bindings have been expanded
  (cond ((or (null? expr) (npair? expr)) expr)
	((free-variable? expr)
	 (let ((val (assoc-ref bl (cadr expr))))
	   (if val val expr)))
	(else (cons (bind-substitute (car expr) bl)
		    (bind-substitute (cdr expr) bl)))))

(define-public (bind-substitute! expr bl)
  "In place substitution of bindings @bl in expression @expr."
  (if (pair? expr)
      (begin
	(if (free-variable? (car expr))
	    (let ((val (assoc-ref bl (cadar expr))))
	      (if val (set-car! expr val)))
	    (bind-substitute! (car expr) bl))
	(if (free-variable? (cdr expr))
	    (let ((val (assoc-ref bl (caddr expr))))
	      (if val (set-cdr! expr val)))
	    (bind-substitute! (cdr expr) bl)))))

(define-public (bind-expand bl2)
  "Recursively substitute the bindings @bl2 in its own values."
  (let ((bl (copy-tree bl2)))
       ; FIXME: would be better to have a cycle-safe copy
    (for-each (lambda (x) (bind-substitute! x bl)) bl)
    bl))
