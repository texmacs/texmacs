
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

(define (make-inactive-label) (make-inactive "label" 1))
(define (make-inactive-reference) (make-inactive "reference" 1))
(define (make-inactive-pageref) (make-inactive "pageref" 1))
(define (make-inactive-write) (make-inactive "write" 1))
(define (make-inactive-action) (make-inactive "action" 2))
(define (make-inactive-hyperlink) 
  (make-inactive-message "hlink" 2 "hyperlink"))
(define (make-inactive-tag) (make-inactive "tag" 2))
(define (make-inactive-meaning) (make-inactive "meaning" 2))

(define (make-include s) (insert-object `(include ,s)))
(define (make-inline-image l)
  (apply make-postscript (cons* (car l) #f (cdr l))))
(define (make-link-image l)
  (apply make-postscript (cons* (car l) #t (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-specific s) (make-inactive-arg "specific" s 2))
(define (make-inactive-superpose) (make-inactive "superpose" 1))
(define (make-inactive-symbol) (make-inactive "symbol" 1))
(define (make-inactive-latex)
  (make-inactive "latex" 1)
  (set-message "Type a latex command followed by return" "latex"))
(define (make-inactive-hybrid)
  (make-inactive "hybrid" 1)
  (set-message
   "tab: insert argument, return: activate or execute latex command"
   "hybrid"))
