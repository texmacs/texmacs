
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mathemagix.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  (det mathemagix-input-det))
