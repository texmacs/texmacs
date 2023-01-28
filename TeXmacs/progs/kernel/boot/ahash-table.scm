
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ahash-table.scm
;; DESCRIPTION : adaptive hash tables
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public make-ahash-table make-hash-table)
(define-public ahash-ref hash-table-ref)
(define-public (ahash-get-handle h s)
  (let ((v  (hash-table-ref h s))) (if v (cons s v) #f)))
(define-public ahash-set! hash-table-set!)
(define-public (ahash-remove! h s) (hash-table-set! h s #f))
(define-public (ahash-table->list h) (map values h))
(define-public (ahash-size h) (length h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines on adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list->ahash-table l)
  (let ((t (make-ahash-table)))
    (for-each (lambda (x) (ahash-set! t (car x) (cdr x))) l)
    t))

(define-public (list->frequencies l)
  (let ((t (make-ahash-table)))
    (for-each (lambda (x) (ahash-set! t x (+ 1 (or (ahash-ref t x) 0)))) l)
    t))

(define-public-macro (ahash-with t var val . body)
  (let ((old-val (gensym))
	(ret-val (gensym)))
    `(with ,old-val (ahash-ref ,t ,var)
       (ahash-set! ,t ,var ,val)
       (with ,ret-val (begin ,@body)
	 (ahash-set! ,t ,var ,old-val)
	 ,ret-val))))

(define-public (ahash-table-invert t)
  (let* ((l (ahash-table->list t))
	 (u (map (lambda (x) (cons (cdr x) (car x))) l)))
    (list->ahash-table u)))

(define-public (ahash-table-append . tl)
  (with ls (map ahash-table->list tl)
    (list->ahash-table (apply append ls))))

(define-public (ahash-table-difference t1 t2)
  (let ((r (make-ahash-table)))
    (for (x (map car (ahash-table->list t1)))
      (when (not (ahash-ref t2 x))
        (ahash-set! r x (ahash-ref t1 x))))
    r))

(define-public (ahash-table-map fun t)
  (let* ((l (ahash-table->list t))
	 (r (map (lambda (x) (cons (car x) (fun (cdr x)))) l)))
    (list->ahash-table r)))

(define-public (ahash-ref* h var val)
  (or (ahash-ref h var) val))

(define-public (ahash-table-select t l)
  (let ((r (make-ahash-table)))
    (for-each (lambda (x)
                (if (ahash-ref t x) (ahash-set! r x (ahash-ref t x)))) l)
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fill-dictionary-entry d key im)
  (if (nnull? key)
      (begin
	(ahash-set! d (car key) im)
	(fill-dictionary-entry d (cdr key) im))))

(define-public (fill-dictionary d l)
  "Fill hash table @d with list of entries @l"
  ;; Note: depreciated
  (if (nnull? l)
      (begin
	(let* ((r (reverse (car l))))
	  (fill-dictionary-entry d (cdr r) (car r)))
	(fill-dictionary d (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple definition of hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (define-table-decls h l)
  (define (insert binding)
    (ahash-set! h (car binding) (cdr binding)))
  (for-each insert l))

(define-public-macro (define-table name . l)
  `(begin
     (when (not (defined? ',name))
       (if (defined? 'tm-define)
           (tm-define ,name (make-ahash-table))
           (define-public ,name (make-ahash-table))))
     (define-table-decls ,name ,(list 'quasiquote l))))

(define-public-macro (extend-table name . l)
  `(define-table-decls ,name ,(list 'quasiquote l)))

(define-public (define-collection-decls h l)
  (define (insert elem)
    (ahash-set! h elem #t))
  (for-each insert l))

(define-public-macro (define-collection name . l)
  `(begin
     (when (not (defined? ',name))
       (if (defined? 'tm-define)
           (tm-define ,name (make-ahash-table))
           (define-public ,name (make-ahash-table))))
     (define-collection-decls ,name ,(list 'quasiquote l))))

(define-public-macro (extend-collection name . l)
  `(define-collection-decls ,name ,(list 'quasiquote l)))
