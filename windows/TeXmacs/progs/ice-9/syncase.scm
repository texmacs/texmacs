;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 


(define-module (ice-9 syncase)
  :use-module (ice-9 debug))



(define-public sc-macro
  (procedure->memoizing-macro
    (lambda (exp env)
      (sc-expand exp))))

;;; Exported variables

(define-public sc-expand #f)
(define-public sc-expand3 #f)
(define-public install-global-transformer #f)
(define-public syntax-dispatch #f)
(define-public syntax-error #f)

(define-public bound-identifier=? #f)
(define-public datum->syntax-object #f)
(define-public define-syntax sc-macro)
(define-public eval-when sc-macro)
(define-public fluid-let-syntax sc-macro)
(define-public free-identifier=? #f)
(define-public generate-temporaries #f)
(define-public identifier? #f)
(define-public identifier-syntax sc-macro)
(define-public let-syntax sc-macro)
(define-public letrec-syntax sc-macro)
(define-public syntax sc-macro)
(define-public syntax-case sc-macro)
(define-public syntax-object->datum #f)
(define-public syntax-rules sc-macro)
(define-public with-syntax sc-macro)
(define-public include sc-macro)

(define primitive-syntax '(quote lambda letrec if set! begin define or
			      and let let* cond do quasiquote unquote
			      unquote-splicing case))

(for-each (lambda (symbol)
	    (set-symbol-property! symbol 'primitive-syntax #t))
	  primitive-syntax)

;;; Hooks needed by the syntax-case macro package

(define-public (void) *unspecified*)

(define andmap
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define (error who format-string why what)
  (start-stack 'syncase-stack
	       (scm-error 'misc-error
			  who
			  "~A ~S"
			  (list why what)
			  '())))

(define the-syncase-module (current-module))

(define (putprop symbol key binding)
  (let* ((m (current-module))
	 (v (or (module-variable m symbol)
		(module-make-local-var! m symbol))))
    (if (assq 'primitive-syntax (symbol-pref symbol))
	(if (eq? (current-module) the-syncase-module)
	    (set-object-property! (module-variable the-root-module symbol)
				  key
				  binding))
	(variable-set! v sc-macro))
    (set-object-property! v key binding)))

(define (getprop symbol key)
  (let* ((m (current-module))
	 (v (module-variable m symbol)))
    (and v (or (object-property v key)
	       (let ((root-v (module-local-variable the-root-module symbol)))
		 (and (equal? root-v v)
		      (object-property root-v key)))))))

(define generated-symbols (make-weak-key-hash-table 1019))

;;; Utilities

(define (psyncomp)
  (system "mv -f psyntax.pp psyntax.pp~")
  (let ((in (open-input-file "psyntax.ss"))
	(out (open-output-file "psyntax.pp")))
    (let loop ((x (read in)))
      (if (eof-object? x)
	  (begin
	    (close-port out)
	    (close-port in))
	  (begin
	    (write (sc-expand3 x 'c '(compile load eval)) out)
	    (newline out)
	    (loop (read in)))))))

;;; Load the preprocessed code

(let ((old-debug #f)
      (old-read #f))
  (dynamic-wind (lambda ()
		  (set! old-debug (debug-options))
		  (set! old-read (read-options)))
		(lambda ()
		  (debug-disable 'debug 'procnames)
		  (read-disable 'positions)
		  (load-from-path "ice-9/psyntax.pp"))
		(lambda ()
		  (debug-options old-debug)
		  (read-options old-read))))


;;; The following line is necessary only if we start making changes
;; (load-from-path "ice-9/psyntax.ss")

(define internal-eval (nested-ref the-scm-module '(app modules guile eval)))

(define-public (eval x)
  (internal-eval (if (and (pair? x)
			  (string=? (car x) "noexpand"))
		     (cadr x)
		     (sc-expand x))))

;;; Hack to make syncase macros work in the slib module
(let ((m (nested-ref the-root-module '(app modules ice-9 slib))))
  (if m
      (set-object-property! (module-local-variable m 'define)
			    '*sc-expander*
			    '(define))))

(define-public syncase sc-expand)
