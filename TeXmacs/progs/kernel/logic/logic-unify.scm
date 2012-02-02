
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-unify.scm
;; DESCRIPTION : unification
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-unify)
  (:use (kernel logic logic-bind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (unify-any l expr bls)
  "Unifications for @l == @expr under any of the bindings in @bls."
  (if (null? bls) '()
      (append (unify l expr (car bls))
	      (unify-any l expr (cdr bls)))))

(define (unify-priority expr)
  (cond ((null? expr) 4)
	((nlist? expr) 3)
	((nlist? (car expr)) 3)
	((free-variable? (car expr)) 1)
	(else 2)))

(define-public (unify l r bl)
  "Unifications for @l == @r under the bindings @bl."
  (let ((lp (unify-priority l))
	(rp (unify-priority r)))
    (cond ((< lp rp) (unify r l bl))
	  ((= rp 4) ; r is the empty list (so that we must have l == r)
	   (list bl))
	  ((= rp 3) ; first element of r is not a list
	   (cond ((null? l) '())
		 ((!= (car l) (car r)) '())
		 (else (unify (cdr l) (cdr r) bl))))
	  ((= rp 2) ; first element of r is again a list
	   (if (or (null? l) (nlist? (car l))) '()
	       (unify-any (cdr l) (cdr r) (unify (car l) (car r) bl))))
	  (else ; first element of r is a free variable
	   (unify-any (cdr l) (cdr r)
		      (bind-unify (cadar r) (car l) bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (logic-unify expr1 expr2)
  "Compute unifications of expressions @expr1 and @expr2 with free variables."
  (let ((sols (unify (list expr1) (list expr2) '())))
    (if (null? sols) #f sols)))
