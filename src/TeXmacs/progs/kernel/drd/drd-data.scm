
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : drd-data.scm
;; DESCRIPTION : Macros for defining TeXmacs data
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel drd drd-data)
  (:use (kernel drd drd-rules) (kernel drd drd-query)))

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

(define drd-facts-table (make-ahash-table))
(define drd-apply-table (make-ahash-table))

(define-public (drd-holds? test . conds)
  "Does the relation @test hold under conditions @conds?"
  (let* ((what (cons test conds))
	 (handle (ahash-get-handle drd-facts-table what)))
    (if handle (cdr handle)
	(let ((ok (== (apply query what) '(()))))
	  (ahash-set! drd-facts-table what ok)
	  ok))))

(define (drd-unique-result l)
  "Retrieve the non-ambiguous functional result from a list of solutions @l."
  (if (null? l) #f
      (let ((bl (car l)))
	(if (!= (length bl) 1)
	    (error "Bad return values for logical apply")
	    (let ((r (cdar bl)))
	      (if (null? (cdr l)) r
		  (if (!= r (drd-unique-result (cdr l)))
		      (error "Ambiguous return values for logical apply")
		      r)))))))

(define-public (drd-apply fun . conds)
  "Retrieve unique @r such that @(rcons fun r) holds under conditions @conds."
  (let* ((what (cons fun conds))
	 (handle (ahash-get-handle drd-apply-table what)))
    (if handle (cdr handle)
	(let* ((goal (cons (append fun '('r)) conds))
	       (val (drd-unique-result (apply query goal))))
	  (ahash-set! drd-apply-table what val)
	  val))))

(define (drd-list-result l)
  "Retrieve the list of results from a list of solutions @l."
  (if (null? l) l
      (let ((bl (car l)))
	(if (not (= (length bl) 1))
	    (error "Bad return values for logical apply")
	    (let ((r (cdar bl)))
	      (cons r (drd-list-result (cdr l))))))))

(define-public (drd-apply-list fun . conds)
  "Retrieve list of @r such that @(rcons fun r) holds under conditions @conds."
  (let* ((what (cons fun conds))
	 (handle (ahash-get-handle drd-apply-table what)))
    (if handle (cdr handle)
	(let* ((goal (cons (append fun '('r)) conds))
	       (val (drd-list-result (apply query goal))))
	  (ahash-set! drd-apply-table what val)
	  val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface for drd-holds? and drd-apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (drd-test? name . args)
  `(drd-holds? (list ,(list 'quasiquote name) ,@args)))

(define-public-macro (drd-lookup name . keys)
  `(drd-apply (list ,(list 'quasiquote name) ,@keys)))

(define-public-macro (drd-lookup-list name . keys)
  `(drd-apply-list (list ,(list 'quasiquote name) ,@keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of groups (should use logical programming soon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (drd-group-rules name l)
  (cond ((null? l) '())
	((is-assume? (car l))
	 (cons (car l) (drd-group-rules name (cdr l))))
	(else
	 (cons (list (list name (car l))) (drd-group-rules name (cdr l))))))

(define-public-macro (drd-group name . l)
  `(drd-rules ,@(drd-group-rules name l)))

(define-public-macro (drd-in? x name . conds)
  `(drd-holds? (list ,(list 'quasiquote name) ,x) ,@(quote-all conds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (drd-table-rule name binding)
  (let ((key (car binding))
	(im (cadr binding))
	(conds (cddr binding)))
    (if (and (pair? key) (== (car key) :or))
	(let ((fun (lambda (skey)
		     (drd-table-rule name (cons skey (cons im conds))))))
	  (apply append (map fun (cdr key))))
	(list (cons (list name key im) conds)))))

(define-public (drd-table-rules name l)
  (cond ((null? l) '())
	((is-assume? (car l))
	 (cons (car l) (drd-table-rules name (cdr l))))
	(else (append (drd-table-rule name (car l))
		      (drd-table-rules name (cdr l))))))

(define-public-macro (drd-table name . l)
  `(drd-rules ,@(drd-table-rules name l)))

(define-public-macro (drd-ref name key . conds)
  `(drd-apply (list ,(list 'quasiquote name) ,key) ,@(quote-all conds)))

(define-public-macro (drd-ref-list name key . conds)
  `(drd-apply-list (list ,(list 'quasiquote name) ,key) ,@(quote-all conds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of dispatchers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (drd-dispatcher-rule rule)
  (if (is-assume? rule) rule
      (cons* (car rule) (list 'unquote (cadr rule)) (cddr rule))))

(define-public-macro (drd-dispatcher name . l)
  `(drd-table ,name ,@(map drd-dispatcher-rule l)))

(define-public-macro (drd-dispatch name key . args)
  (let ((k (gensym)))
    (if (= (length args) 1)
	`(let ((,k ,key))
	   ((drd-ref ,name (car ,k)) ,k))
	`((drd-ref ,name ,key) ,@args))))
