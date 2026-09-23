
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prologue.scm
;; DESCRIPTION : subroutines which are not well implemented in guile
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot prologue)
  (:use (kernel boot ahash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work around broken 'symbol-property'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol-procedure-table (make-ahash-table))
(define symbol-property-table (make-ahash-table))

(define-public (set-symbol-procedure! symb proc)
  (ahash-set! symbol-procedure-table symb proc))

(define-public (symbol-procedure symb)
  (ahash-ref symbol-procedure-table symb))

(define-public (set-symbol-prop! symb prop val)
  (ahash-set! symbol-property-table (list symb prop) val))

(define-public (symbol-prop symb prop)
  (ahash-ref symbol-property-table (list symb prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-sort-merge l1 l2 comp?)
  (cond ((null? l1) l2)
	((null? l2) l1)
	((comp? (car l1) (car l2))
	 (cons (car l1) (list-sort-merge (cdr l1) l2 comp?)))
	(else (cons (car l2) (list-sort-merge l1 (cdr l2) comp?)))))

(define-public (list-sort l comp?)
  "Merge sort of @l using the comparison @comp?."
  (with n (length l)
    (if (< n 2) l
	(let* ((m  (quotient n 2))
	       (ll (list-sort (list-head l m) comp?))
	       (rl (list-sort (list-tail l m) comp?)))
	  (list-sort-merge ll rl comp?)))))
