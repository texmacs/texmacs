
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : giac-input.scm
;; DESCRIPTION : Giac input converters
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (giac-input)
  (:use (texmacs plugin plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (giac-input-var-row r)
  (if (not (null? r))
      (begin
	(display ",")
	(plugin-input (car r))
	(giac-input-var-row (cdr r)))))

(define (giac-input-row r)
  (display "[")
  (plugin-input (car r))
  (giac-input-var-row (cdr r))
  (display "]"))

(define (giac-input-var-rows t)
  (if (not (null? t))
      (begin
	(display ",")
	(giac-input-row (car t))
	(giac-input-var-rows (cdr t)))))

(define (giac-input-rows t)
  (display "[")
  (giac-input-row (car t))
  (giac-input-var-rows (cdr t))
  (display "]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters giac
  (rows giac-input-rows))
