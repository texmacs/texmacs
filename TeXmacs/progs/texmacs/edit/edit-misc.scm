
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
    ;; linking related content
    make-inactive-label make-inactive-reference make-inactive-pageref
    make-inactive-write make-inactive-action make-inactive-hyperlink
    make-inactive-tag make-inactive-meaning
    make-include make-inline-image make-link-image
    ;; miscellaneous content
    make-inactive-specific make-inactive-superpose
    make-inactive-symbol make-inactive-latex make-inactive-hybrid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for linking related content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-label) (make 'label))
(define (make-inactive-reference) (make 'reference))
(define (make-inactive-pageref) (make 'pageref))
(define (make-inactive-write) (make 'write))
(define (make-inactive-action) (make 'action))
(define (make-inactive-hyperlink) (make 'hlink))
(define (make-inactive-tag) (make 'tag))
(define (make-inactive-meaning) (make 'meaning))

(define (make-include s) (insert-object `(include ,s)))
(define (make-inline-image l)
  (apply make-postscript (cons* (car l) #f (cdr l))))
(define (make-link-image l)
  (apply make-postscript (cons* (car l) #t (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-specific s)
  (if (or (== s "texmacs") (in-preamble?))
      (insert-object-go-to `(specific ,s "") '(1 0))
      (insert-object-go-to `(inactive (specific ,s "")) '(0 1 0))))
(define (make-inactive-superpose) (make 'superpose))
(define (make-inactive-symbol) (make 'symbol))
(define (make-inactive-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))
(define (make-inactive-hybrid)
  (make 'hybrid)
  (set-message
   "A-right: insert argument, return: activate or execute latex command"
   "hybrid"))
