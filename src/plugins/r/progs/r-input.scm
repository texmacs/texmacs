
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : r-input.scm
;; DESCRIPTION : R input converters
;; COPYRIGHT   : (C) 1999  Michael Lachmann and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (r-input)
  (:use (texmacs plugin plugin-convert)))

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
