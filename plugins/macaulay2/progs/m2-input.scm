
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : m2-input.scm
;; DESCRIPTION : Macaulay2 input converters
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (m2-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macaulay2-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(macaulay2-input-var-row (cdr r)))))

(define (macaulay2-input-row r)
  (display "{")
  (plugin-input (car r))
  (macaulay2-input-var-row (cdr r))
  (display "}"))

(define (macaulay2-input-var-rows t)
  (if (nnull? t)
      (begin
	(display ", ")
	(macaulay2-input-row (car t))
	(macaulay2-input-var-rows (cdr t)))))

(define (macaulay2-input-rows t)
  (display "matrix({")
  (macaulay2-input-row (car t))
  (macaulay2-input-var-rows (cdr t))
  (display "})"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters macaulay2
  (rows macaulay2-input-rows))
