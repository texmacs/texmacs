
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-secure.scm
;; DESCRIPTION : Secure evaluation of Scheme scripts
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-secure)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-plugins)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive secure functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (define-secure-symbols . l)
  (for-each (lambda (x) (property-set! x :secure #t '())) l)
  '(noop))

(define-secure-symbols
  boolean? null? symbol? string? pair? list?
  equal? == not
  string-length substring string-append
  string->list list->string string-ref string-set!
  + - * / gcd lcm quotient remainder modulo abs log exp sqrt
  car cdr caar cadr cdar cddr
  caaar caadr cadar caddr cdaar cdadr cddar cdddr
  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
  cons list append length reverse
  texmacs-version texmacs-version-release*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secure-args? args env)
  (if (null? args) #t
      (and (secure-expr? (car args) env)
	   (secure-args? (cdr args) env))))

(define (secure-cond? args env)
  (if (null? args) #t
      (and (or (== (caar args) 'else) (secure-expr? (caar args) env))
	   (secure-expr? (cadar args) env)
	   (secure-cond? (cdr args) env))))

(define (local-env env l)
  (cond ((null? l) env)
	((pair? l) (local-env (assoc-set! env (car l) #t) (cdr l)))
	(else (assoc-set! env l #t))))

(define (secure-lambda? args env)
  (secure-args? (cdr args) (local-env env (car args))))

(define (secure-quasiquote? args env)
  (cond ((npair? args) #t)
	((func? args 'unquote 1) (secure-expr? (cadr args) env))
	((func? args 'unquote-splicing 1) (secure-expr? (cadr args) env))
	(else (and (secure-quasiquote? (car args) env)
		   (secure-quasiquote? (cdr args) env)))))

(define (secure-expr? expr env)
  (cond ((pair? expr)
	 (let* ((f (car expr))
		(m (logic-ref secure-macros% f)))
	   (cond (m (m (cdr expr) env))
		 ((assoc-ref env f) (secure-args? (cdr expr) env))
		 ((== f 'quote) #t)
		 ((== f 'quasiquote) (secure-quasiquote? (cdr expr) env))
		 ((symbol? f)
		  (and (property f :secure)
		       (secure-args? (cdr expr) env)))
		 (else (secure-args? expr env)))))
	((symbol? expr) #t)
	((number? expr) #t)
	((string? expr) #t)
	((tree? expr) #t)
	((null? expr) #t)
	((boolean? expr) #t)
	(else #f)))

(logic-table secure-macros%
  (and ,secure-args?)
  (begin ,secure-args?)
  (cond ,secure-cond?)
  (if ,secure-args?)
  (lambda ,secure-lambda?)
  (or ,secure-args?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (secure? expr)
  "Test whether it is secure to evaluate the expression @expr"
  (or (secure-expr? expr '())
      (and (lazy-plugin-force) (secure-expr? expr '()))))
