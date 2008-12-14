
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r-input.scm
;; DESCRIPTION : R input converters
;; COPYRIGHT   : (C) 1999  Michael Lachmann and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (r-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(r-input-var-row (cdr r)))))

(define (r-input-row r)
  (plugin-input (car r))
  (r-input-var-row (cdr r))
 )

(define (r-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(r-input-row (car t))
	(r-input-var-rows (cdr t)))))

(define (r-input-rows t)
  (display "t(matrix(c(")
  (r-input-row (car t))
  (r-input-var-rows (cdr t))
  (display "), ")
  (display (length (car t)))
  (display ", ")
  (display (length t))
  (display " ))"))

(define (r-input-det args)
  (display "det(")
    (plugin-input-descend-last args)
      (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters r
  (rows r-input-rows)
  (det r-input-det)
  (sum r-input-det)

  ("<vee>" " | ")
  ("<wedge>" " & ")
  ("<leftarrow>" " <less>- ")
  ("<infty>" " Inf ")
  ("<varnothing>" "NULL")
  ("<cdots>" ":")
  ("<ldots>" ":"))
