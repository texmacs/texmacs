
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrevs.scm
;; DESCRIPTION : useful abbreviations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tools abbrevs))
(use-modules (tools base))

(define-public == equal?)
(define-public (!= x y) (not (equal? x y)))

(define-public (nstring? x) (not (string? x)))
(define-public (nnull? x) (not (null? x)))
(define-public (npair? x) (not (pair? x)))
(define-public (nlist? x) (not (list? x)))
(define-public (nnot x) (not (not x)))
(define-public-macro (toggle! x) `(set! ,x (not ,x)))

(define-public (list-1? x) (and (pair? x) (null? (cdr x))))
(define-public (nlist-1? x) (not (list-1? x)))
(define-public (list-2? x) (and (list? x) (= (length x) 2)))
(define-public (nlist-2? x) (not (list-2? x)))
(define-public (list-3? x) (and (list? x) (= (length x) 3)))
(define-public (nlist-3? x) (not (list-3? x)))

(define-public (in? x l) (not (not (member x l))))
(define-public (nin? x l) (not (member x l)))

(define-public (always? . l) #t)
(define-public (never? . l) #f)
(define-public (root? t) (== (reverse (tree-ip t)) (buffer-path)))
(define-public (nroot? t) (!= (reverse (tree-ip t)) (buffer-path)))
(define-public (leaf? t) (== (tree-ip t) (cdr (reverse (cursor-path)))))
(define-public (nleaf? t) (!= (tree-ip t) (cdr (reverse (cursor-path)))))
(define-public (true? . l) #t)
(define-public (false? . l) #f)

(define-public (identity x) x)

(define-public (keyword->number x)
  (string->number (symbol->string (keyword->symbol x))))
(define-public (number->keyword x)
  (symbol->keyword (string->symbol (number->string x))))

(define-public (object->string obj)
  (call-with-output-string
    (lambda (port) (write obj port))))

(define-public (string->object s)
  (call-with-input-string s read))

(define-public (display* . l)
  (for-each display l))

(define-public (symbol<=? x y)
  (string<=? (symbol->string x) (symbol->string y)))

(define-public call/cc call-with-current-continuation)
(define-public-macro (with-cc cont . body)
  `(call/cc (lambda (,cont) ,@body)))

(define-public-macro (when cond . body)
  `(if ,cond (begin ,@body) #f))

(define-public-macro (with var val . body)
  (if (pair? var)
      `(apply (lambda ,var ,@body) ,val)
      `(let ((,var ,val)) ,@body)))

(define-public-macro (with-global var val . body)
  (let ((old (gensym)) (new (gensym)))
    `(let ((,old ,var))
       (set! ,var ,val)
       (let ((,new (begin ,@body)))
	 (set! ,var ,old)
	 ,new))))

(define-public-macro (and-with var val . body)
  `(with ,var ,val
     (and ,var (begin ,@body))))

(define-public (.. start end)
  (if (< start end)
      (cons start (.. (1+ start) end))
      '()))

(define-public-macro (for what . body)
  (cond ((list-2? what)
	 `(for-each (lambda (,(car what)) ,@body)
		    ,(cadr what)))
	((list-3? what)
	 `(for-each (lambda (,(car what)) ,@body)
		    (.. ,(cadr what) ,(caddr what))))
	(else '(noop))))

(define-public-macro (repeat n . body)
  (let ((x (gensym)))
    `(for (,x 0 ,n) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION : and-let* special form
;;
;; COPYRIGHT : 2001, Free Software Foundation, Inc.
;; The copyright of the reference implementation of SRFI-2 by Oleg Kiselyov was
;; assigned to the Free Software Foundation in Feb. 2001. The following
;; implementation also includes incidental changes by Dale Jordan.
;; Modified by David Allouche to use syntax-error procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scm-error* type caller message . opt)
  (apply scm-error type caller message opt))

(define (syntax-error where message . args)
  (scm-error* 'syntax-error where message args #f))

(define-public-macro (and-let* claws . body)
  (let* ((new-vars '())
	 (result (cons 'and '()))
	 (growth-point result))

    (define (andjoin! clause)
      (let ((prev-point growth-point)
	    (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
	(syntax-error "and-let*" "Bindings are not a list: ~A" claws))
    (for-each
     (lambda (claw)
       (cond
	((symbol? claw)                         ; BOUND-VARIABLE form
	 (andjoin! claw))
	((and (pair? claw) (null? (cdr claw)))  ; (EXPRESSION) form
	 (andjoin! (car claw)))
	((and (pair? claw) (symbol? (car claw)) ; (VARIABLE EXPRESSION) form
	      (pair? (cdr claw)) (null? (cddr claw)))
	 (let* ((var (car claw))
		(var-cell (cons var '())))
	   (if (memq var new-vars)
	       (syntax-error "and-let*"
			     "Duplicate variable in bindings: ~A" var))
	   (set! new-vars (cons var new-vars))
	   (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
	   (set! growth-point var-cell)))
	(else
	 (syntax-error "and-let*" "Ill-formed binding: ~A" claw))))
     claws)
    (if (not (null? body))
	(if (null? (cdr body))
	    (andjoin! (car body))
	    (andjoin! `(begin ,@body))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION : curry which is not curry (SRFI-26)
;; COPYRIGHT : 2000, Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from GLUG repository. Author: Daniel Skarda <0rfelyus@ucw.cz>

(define-public-macro (cut slot . slots)
  (let loop ((slots	(cons slot slots))
	     (params	'())
	     (args	'()))
    (if (null? slots)
	`(lambda ,(reverse! params) ,(reverse! args))
        (let ((s	  (car slots))
              (rest (cdr slots)))
          (case s
            ((<>)
             (let ((var (gensym)))
               (loop rest (cons var params) (cons var args))))
            ((<...>)
             (if (pair? rest)
                 (error "<...> not on the end of cut expression"))
             (let ((var (gensym)))
               `(lambda ,(append! (reverse! params) var)
                  (apply ,@(reverse! (cons var args))))))
            (else
             (loop rest params (cons s args))))))))
