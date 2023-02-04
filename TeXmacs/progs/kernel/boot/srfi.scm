
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : srfi.scm
;; DESCRIPTION : special keywords collected from elsewhere
;; COPYRIGHT   : [see copyright statement of each SECTION]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot srfi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION : and-let* special form
;;
;; COPYRIGHT : 2001, Free Software Foundation, Inc.
;; The copyright of the reference implementation of SRFI-2 by Oleg Kiselyov was
;; assigned to the Free Software Foundation in Feb. 2001. The following
;; implementation also includes incidental changes by Dale Jordan.
;; Modified by David Allouche to use TeXmacs syntax-error procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; and-let* is a generalized and: it evaluates a sequence of forms one
;; after another till the first one that yields #f; the non-#f result
;; of a form can be bound to a fresh variable and used in the
;; subsequent forms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Please note that Guile version 1.6.0 and higher provide AND-LET* support
;; but it does not provide as good an error reporting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; SECTION : receive special form (SRFI-8)
;; COPYRIGHT : 2000, 2001 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from guile-1.6.0.

(define-public-macro (receive vars vals . body)
  `((lambda ,vars ,@body) ,vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION : case-lambda special form (SRFI-16)
;; COPYRIGHT : 2000, 2001 Free Software Foundation, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from guile-1.6.0. Author: Martin Grabmueller

(define-public-macro (case-lambda . clauses)

  ;; Return the length of the list @var{l}, but allow dotted list.
  (define (alength l)
    (cond ((null? l) 0)
	  ((pair? l) (+ 1 (alength (cdr l))))
	  (else 0)))

  ;; Return @code{#t} if @var{l} is a dotted list, @code{#f} if it is a normal
  ;; list.
  (define (dotted? l)
    (cond ((null? l) #f)
	  ((pair? l) (dotted? (cdr l)))
	  (else #t)))

  ;; Return the expression for accessing the @var{index}th element of the list
  ;; called @var{args-name}. If @var{tail?} is true, code for accessing the
  ;; list-tail is generated, otherwise for accessing the list element itself.
  (define (accessor args-name index tail?)
    (if tail?
	(case index
	  ((0) `,args-name)
	  ((1) `(cdr ,args-name))
	  ((2) `(cddr ,args-name))
	  ((3) `(cdddr ,args-name))
	  ((4) `(cddddr ,args-name))
	  (else `(list-tail ,args-name ,index)))
	(case index
	  ((0) `(car ,args-name))
	  ((1) `(cadr ,args-name))
	  ((2) `(caddr ,args-name))
	  ((3) `(cadddr ,args-name))
	  (else `(list-ref ,args-name ,index)))))

  ;; Generate the binding lists of the variables of one case-lambda clause.
  ;; @var{vars} is the (possibly dotted) list of variables and @var{args-name}
  ;; is the generated name used for the argument list.
  (define (gen-temps vars args-name)
    (let lp ((v vars) (i 0))
      (cond ((null? v) '())
	    ((pair? v) 
	     (cons `(,(car v) ,(accessor args-name i #f))
		   (lp (cdr v) (+ i 1))))
	    (else `((,v ,(accessor args-name i #t)))))))

  ;; Generate the cond clauses for each of the clauses of case-lambda,
  ;; including the parameter count check, binding of the parameters and the
  ;; code of the corresponding body.
  (define (gen-clauses l length-name args-name)
    (cond ((null? l) (list '(else (error "too few arguments"))))
	  (else
	   (cons
	    `((,(if (dotted? (caar l)) '>= '=)
	       ,length-name ,(alength (caar l)))
	      (let ,(gen-temps (caar l) args-name)
	      ,@(cdar l)))
	    (gen-clauses (cdr l) length-name args-name)))))

  (let ((args-name (gensym))
	(length-name (gensym)))
    (let ((proc
	   `(lambda ,args-name
	      (let ((,length-name (length ,args-name)))
		(cond ,@(gen-clauses clauses length-name args-name))))))
      proc)))

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

(define-public-macro (cute . slots)
  (let ((temp
	 (map (lambda (s) (and (not (memq s '(<> <...>))) (gensym))) slots)))
    `(let ,(delq #f (map (lambda (t s) (and t (list t s))) temp slots))
       (cut ,@(map (lambda (t s) (or t s)) temp slots)))))
