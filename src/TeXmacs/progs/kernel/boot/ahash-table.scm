
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ahash-table.scm
;; DESCRIPTION : adaptive hash tables
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot ahash-table)
  (:export
    make-ahash-table ahash-ref ahash-get-handle ahash-size
    ahash-set! ahash-remove!
    ahash-fold ahash-table->list list->ahash-table
    define-table-decls ;; for define-table macro
    define-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (vector? (make-hash-table 1))
    (begin ;; old style
      (define (make-ahash-table)
	(cons (make-hash-table 1) 0))

      (define (ahash-ref h key)
	(hash-ref (car h) key))

      (define (ahash-get-handle h key)
	(hash-get-handle (car h) key))

      (define (ahash-size h)
	(cdr h))

      (define (ahash-slots! h new-size)
	(let ((new-h (make-hash-table new-size)))
	  (hash-fold (lambda (key value dummy) (hash-set! new-h key value))
		     #f (car h))
	  (set-car! h new-h)))

      (define (ahash-set! h key value)
	(if (hash-get-handle (car h) key)
	    (hash-set! (car h) key value)
	    (begin
	      (if (>= (cdr h) (vector-length (car h)))
		  (ahash-slots! h (+ (* 2 (vector-length (car h))) 1)))
	      (set-cdr! h (+ (cdr h) 1))
	      (hash-set! (car h) key value))))

      (define (ahash-remove! h key)
	(let ((removed (hash-remove! (car h) key)))
	  (if removed
	      (begin
		(set-cdr! h (- (cdr h) 1))
		(if (< (+ (* 4 (cdr h)) 1) (vector-length (car h)))
		    (ahash-slots! h (quotient (vector-length (car h)) 2)))))
	  removed))

      (define (ahash-fold folder init h)
	(hash-fold folder init (car h)))

      (define (ahash-table->list h)
	(hash-fold acons '() (car h))))

    (begin ;; new style
      (define make-ahash-table make-hash-table)
      (define ahash-ref hash-ref)
      (define ahash-get-handle hash-get-handle)
      (define (ahash-size h)
	(hash-fold (lambda (key value seed) (+ 1 seed)) 0 h))
      (define ahash-set! hash-set!)
      (define ahash-remove! hash-remove!)
      (define ahash-fold hash-fold)
      (define (ahash-table->list h)
	(hash-fold acons '() h))))

(define (list->ahash-table l)
  (let ((t (make-ahash-table)))
    (for-each (lambda (x) (ahash-set! t (car x) (cdr x))) l)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple definition of hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-table-decls h l)
  (define (insert binding)
    (ahash-set! h (car binding) (cdr binding)))
  (for-each insert l))

(define-macro (define-table name . l)
  `(begin
     (define ,name (make-ahash-table))
     (define-table-decls ,name ,(list 'quasiquote l))))
