
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-data.scm
;; DESCRIPTION : Macros for defining TeXmacs data
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-data)
  (:use (kernel logic logic-rules) (kernel logic logic-query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-assume? expr)
  (and (pair? expr) (== (car expr) 'assume)))

(define (quote-all l)
  (map (lambda (x) (list 'quote x)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extraction of logical information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logic-facts-table (make-ahash-table))
(define logic-apply-table (make-ahash-table))

(define-public (logic-holds? test . conds)
  "Does the relation @test hold under conditions @conds?"
  (let* ((what (cons test conds))
	 (handle (ahash-get-handle logic-facts-table what)))
    (if handle (cdr handle)
	(let ((ok (== (apply query what) '(()))))
	  (ahash-set! logic-facts-table what ok)
	  ok))))

(define (logic-unique-result l)
  "Retrieve the non-ambiguous functional result from a list of solutions @l."
  (if (null? l) #f
      (let ((bl (car l)))
	(if (!= (length bl) 1)
	    (error "Bad return values for logical apply")
	    (let ((r (cdar bl)))
	      (if (null? (cdr l)) r
		  (if (!= r (logic-unique-result (cdr l)))
		      (error "Ambiguous return values for logical apply")
		      r)))))))

(define-public (logic-apply fun . conds)
  "Retrieve unique @r such that @(rcons fun r) holds under conditions @conds."
  (let* ((what (cons fun conds))
	 (handle (ahash-get-handle logic-apply-table what)))
    (if handle (cdr handle)
	(let* ((goal (cons (append fun '('r)) conds))
	       (val (logic-unique-result (apply query goal))))
	  (ahash-set! logic-apply-table what val)
	  val))))

(define (logic-list-result l)
  "Retrieve the list of results from a list of solutions @l."
  (if (null? l) l
      (let ((bl (car l)))
	(if (not (= (length bl) 1))
	    (error "Bad return values for logical apply")
	    (let ((r (cdar bl)))
	      (cons r (logic-list-result (cdr l))))))))

(define-public (logic-apply-list fun . conds)
  "Retrieve list of @r such that @(rcons fun r) holds under conditions @conds."
  (let* ((what (cons fun conds))
	 (handle (ahash-get-handle logic-apply-table what)))
    (if handle (cdr handle)
	(let* ((goal (cons (append fun '('r)) conds))
	       (val (logic-list-result (apply query goal))))
	  (ahash-set! logic-apply-table what val)
	  val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface for logic-holds? and logic-apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (logic-test? name . args)
  `(logic-holds? (list ,(list 'quasiquote name) ,@args)))

(define-public-macro (logic-lookup name . keys)
  `(logic-apply (list ,(list 'quasiquote name) ,@keys)))

(define-public-macro (logic-lookup-list name . keys)
  `(logic-apply-list (list ,(list 'quasiquote name) ,@keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of groups (should use logical programming soon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (logic-group-rules name l)
  (cond ((null? l) '())
	((is-assume? (car l))
	 (cons (car l) (logic-group-rules name (cdr l))))
	(else
	 (cons (list (list name (car l))) (logic-group-rules name (cdr l))))))

(define-public-macro (logic-group name . l)
  `(logic-rules ,@(logic-group-rules name l)))

(define-public-macro (logic-in? x name . conds)
  `(logic-holds? (list ,(list 'quasiquote name) ,x) ,@(quote-all conds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logic-table-rule name binding)
  (let ((key (car binding))
	(im (cadr binding))
	(conds (cddr binding)))
    (if (and (pair? key) (== (car key) :or))
	(let ((fun (lambda (skey)
		     (logic-table-rule name (cons skey (cons im conds))))))
	  (apply append (map fun (cdr key))))
	(list (cons (list name key im) conds)))))

(define-public (logic-table-rules name l)
  (cond ((null? l) '())
	((is-assume? (car l))
	 (cons (car l) (logic-table-rules name (cdr l))))
	(else (append (logic-table-rule name (car l))
		      (logic-table-rules name (cdr l))))))

(define-public-macro (logic-table name . l)
  `(logic-rules ,@(logic-table-rules name l)))

(define-public-macro (logic-ref name key . conds)
  ;;`(logic-apply (list ,(list 'quasiquote name) ,key) ,@(quote-all conds)))
  `(logic-apply (list ,(list 'quasiquote name) ,key) ,@conds))

(define-public-macro (logic-ref-list name key . conds)
  ;;`(logic-apply-list (list ,(list 'quasiquote name) ,key) ,@(quote-all conds)))
  `(logic-apply-list (list ,(list 'quasiquote name) ,key) ,@conds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of dispatchers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (logic-dispatcher-rule rule)
  (if (is-assume? rule) rule
      (cons* (car rule) (list 'unquote (cadr rule)) (cddr rule))))

(define-public-macro (logic-dispatcher name . l)
  `(logic-table ,name ,@(map logic-dispatcher-rule l)))

(define-public-macro (logic-dispatch name key . args)
  (let ((k (gensym)))
    (if (= (length args) 1)
	`(let ((,k ,key))
	   ((logic-ref ,name (car ,k)) ,k))
	`((logic-ref ,name ,key) ,@args))))
