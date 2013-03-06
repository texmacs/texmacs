
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : sage-input.scm
;; DESCRIPTION : Sage input converters
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (sage-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sage-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(sage-input-var-row (cdr r)))))

(define (sage-input-row r)
  (display "[")
  (plugin-input (car r))
  (sage-input-var-row (cdr r))
  (display "]"))

(define (sage-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(sage-input-row (car t))
	(sage-input-var-rows (cdr t)))))

(define (sage-input-rows t)
  (display "matrix([")
  (sage-input-row (car t))
  (sage-input-var-rows (cdr t))
  (display "])"))

(define (sage-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (sage-input-descend-last (cdr args))))

(define (sage-input-det args)
  (display "det(")
  (sage-input-descend-last args)
  (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters sage
  (rows sage-input-rows)
  (det sage-input-det))
