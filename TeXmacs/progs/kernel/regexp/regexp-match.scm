
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : regexp-match.scm
;; DESCRIPTION : pattern matching
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module provides a powerful routine for pattern matching for
;; scheme trees, trees and strings. The patterns may either be regular
;; expressions, user defined grammars, or user defined predicates.
;; Moreover, patterns may contain wildcard variables, and the list
;; of all possible matches is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel regexp regexp-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intersections and unions of solution sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (bindings-add bl var val)
  "Bind variable @var to @val in @bl if possible."
  (cond ((assoc-ref bl var)
	 (if (== (assoc-ref bl var) val) bl #f))
	(else (cons (cons var val) bl))))

(define (match-any l pat bls)
  "Matches for @l == @pat under any of the bindings in @bls."
  (if (null? bls) '()
      (append (match l pat (car bls))
	      (match-any l pat (cdr bls)))))

(define-macro (match-cup expr1 expr2)
  `(let ((l1 ,expr1))
     (if (== l1 '(())) '(())
	 (let ((l2 ,expr2))
	   (if (== l2 '(())) '(())
	       (append l1 l2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (match-or l args pat bl)
  "Matches for @l == @((:or . args) . pat) under bindings @bl."
  (if (null? args) '()
      (match-cup (match l (cons (car args) pat) bl)
		 (match-or l (cdr args) pat bl))))

(define (match-and l args pat bl)
  "Matches for @l == @((:and . args) . pat) under bindings @bl."
  (if (null? args) (list bl)
      (match-any l (cons (car args) pat)
		 (match-and l (cdr args) pat bl))))

(define (match-not l args pat bl)
  "Matches for @l == @((:not . args) . pat) under bindings @bl."
  ;; WARNING: the behaviour of this routine w.r.t. bindings
  ;; has not been investigated in detail
  (if (or (null? l) (!= (length args) 1)
	  (nnull? (match l (append args pat) bl)))
      '()
      (match (cdr l) pat bl)))

(define (match-repeat l args pat bl)
  "Matches for @l == @((:repeat . args) . pat) under bindings @bl."
  (if (null? args) '()
      (match-cup (match l pat bl)
		 (match l (append args (cons (cons :repeat args) pat)) bl))))

(define (match-group l args pat bl)
  "Matches for @l == @((:group . args) . pat) under bindings @bl."
  (match l (append args pat) bl))

(define (match-quote l args pat bl)
  "Matches for @l == @((:quote . args) . pat) under bindings @bl."
  (if (or (null? l) (!= (length args) 1) (!= (car l) (car args)))
      '()
      (match (cdr l) pat bl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public match-term (make-ahash-table))
(define match-table (make-ahash-table))
(ahash-set! match-table :or match-or)
(ahash-set! match-table :and match-and)
(ahash-set! match-table :not match-not)
(ahash-set! match-table :repeat match-repeat)
(ahash-set! match-table :group match-group)
(ahash-set! match-table :quote match-quote)

(define-public (match l pat bl)
  "Matches for @l == @pat under bindings @bl."
  (if (null? pat) (if (== l '()) (list bl) '())
      (let ((fpat (car pat)))
	(cond ((keyword? fpat)
	       (let* ((symb (keyword->symbol fpat))
		      (n (string->number
			  (string-tail
			   (symbol->string symb) 1))))
		 (cond ((== fpat :*) (list bl))
		       (n (if (>= (length l) n)
			      (match (list-tail l n) (cdr pat) bl)
			      '()))
		       ((null? l) '())
		       ((ahash-ref match-term fpat)
			(with upat (ahash-ref match-term fpat)
			  (match l (append upat (cdr pat)) bl)))
		       ((not (apply (eval symb) (list (car l)))) '())
		       (else (match (cdr l) (cdr pat) bl)))))
	      ((and (list? fpat) (nnull? fpat))
	       (let ((ffpat (car fpat)))
		 (cond ((and (keyword? ffpat) (ahash-ref match-table ffpat))
			(apply (ahash-ref match-table ffpat)
			       (list l (cdr fpat) (cdr pat) bl)))
		       ((or (nlist? l) (null? l)) '())
		       ((== ffpat 'quote)
			(let ((new-bl (bindings-add bl (cadr fpat) (car l))))
			  (if new-bl (match (cdr l) (cdr pat) new-bl) '())))
		       ((list? (car l))
			(match-any (cdr l) (cdr pat)
				   (match (car l) fpat bl)))
		       ((compound-tree? (car l))
			(match-any (cdr l) (cdr pat)
				   (match (tree->list (car l)) fpat bl)))
		       ((string? (car l))
			(match-any (cdr l) (cdr pat)
				   (match (string->list (car l)) fpat bl)))
		       (else '()))))
	      ((null? l) '())
	      ((not (tm-equal? (car l) fpat)) '())
	      (else (match (cdr l) (cdr pat) bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (define-regexp-grammar-decls l)
  (define (insert rule)
    (ahash-set! match-term (car rule) (cdr rule)))
  (for-each insert l))

(define-public-macro (define-regexp-grammar . l)
  `(begin
     (define-regexp-grammar-decls ,(list 'quasiquote l))))

(define-public (match? x pattern)
  "Does @x match the pattern @pat?"
  (let ((sols (match (list x) (list pattern) '())))
    (if (null? sols) #f sols)))
