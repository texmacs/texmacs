
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : misc.scm
;; DESCRIPTION : inserting miscellaneous content
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-misc)
  (:export
    make-specific make-latex
    make-include make-inline-image make-link-image))

(define (make-specific s)
  (if (or (== s "texmacs") (in-preamble?))
      (insert-object-go-to `(specific ,s "") '(1 0))
      (insert-object-go-to `(inactive (specific ,s "")) '(0 1 0))))

(define (make-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))

(define (make-include s)
  (insert-object `(include ,s)))

(define (make-inline-image l)
  (apply make-postscript (cons* (car l) #f (cdr l))))

(define (make-link-image l)
  (apply make-postscript (cons* (car l) #t (cdr l))))
