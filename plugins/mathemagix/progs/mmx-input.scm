
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mmx-input.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mmx-input)
  (:use (utils plugins plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mmx-input-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(mmx-input-var-row (cdr r)))))

(tm-define (mmx-input-row r)
  (plugin-input (car r))
  (mmx-input-var-row (cdr r)))

(tm-define (mmx-input-var-rows t)
  (if (nnull? t)
      (begin
	(display "; ")
	(mmx-input-row (car t))
	(mmx-input-var-rows (cdr t)))))

(tm-define (mmx-input-rows t)
  (display "matrix(")
  (mmx-input-row (car t))
  (mmx-input-var-rows (cdr t))
  (display ")"))

(tm-define (mmx-input-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (mmx-input-descend-last (cdr args))))

(tm-define (mmx-input-det args)
  (display "det(")
  (mmx-input-descend-last args)
  (display ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (mmx-converters mmx)
  `(plugin-input-converters ,mmx
     (rows mmx-input-rows)
     (det mmx-input-det)
     ("<in>" " in ")
     ("<neg>" "!")
     ("<wedge>" "/\\")
     ("<vee>" "\\/")
     ("<mapsto>" ":-<gtr>")))
