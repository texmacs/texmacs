
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mathemagix.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mathemagix-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathemagix-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(mathemagix-input-var-row (cdr r)))))

(define (mathemagix-input-row r)
  (plugin-input (car r))
  (mathemagix-input-var-row (cdr r)))

(define (mathemagix-input-var-rows t)
  (if (nnull? t)
      (begin
	(display "; ")
	(mathemagix-input-row (car t))
	(mathemagix-input-var-rows (cdr t)))))

(define (mathemagix-input-rows t)
  (display "matrix(")
  (mathemagix-input-row (car t))
  (mathemagix-input-var-rows (cdr t))
  (display ")"))

(define (mathemagix-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (mathemagix-input-descend-last (cdr args))))

(define (mathemagix-input-det args)
  (display "det(")
  (mathemagix-input-descend-last args)
  (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters mathemagix
  (rows mathemagix-input-rows)
  (det mathemagix-input-det)
  ("<in>" " in ")
  ("<neg>" "!")
  ("<wedge>" "/\\")
  ("<vee>" "\\/")
  ("<mapsto>" ":-<gtr>"))
