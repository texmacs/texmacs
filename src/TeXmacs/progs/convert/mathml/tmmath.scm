
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmath.scm
;; DESCRIPTION : conversion of TeXmacs trees into MathML trees
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml tmmath)
  (:use (convert tools tmconcat))
  (:export texmacs->mathml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Empty handler and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-noop l) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-concat-explode x)
  (if (string? x)
      (tmconcat-tokenize-math x)
      (list x)))

(define (tmmath-concat l)
  (let* ((l2 (apply append (map tmmath-concat-explode l)))
	 (l3 ()))
    ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-frac l)
  `(m:mfrac ,(tmmath (car l)) ,(tmmath (cadr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-dispatch htable l)
  (let ((x (drd-ref ,htable (car l))))
    (and (procedure? x)
	 (x (cdr l)))))

(define (tmmath x)
  (if (string? x)
      (tmmath-concat `(concat ,x))
      (or (tmmath-dispatch 'tmmath-primitives% x)
	  "?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmmath-primitives%
  (concat tmmath-concat)
  ;(group tmmath-id)
  ;(left tmmath-id)
  ;(mid tmmath-id)
  ;(right tmmath-id)
  ;(big tmmath-big)
  ;(lprime tmmath-id)
  ;(rprime tmmath-id)
  ;(below tmmath-below)
  ;(above tmmath-above)
  ;(lsub tmmath-sub)
  ;(lsup tmmath-sup)
  ;(rsub tmmath-sub)
  ;(rsup tmmath-sup)
  (frac tmmath-frac)
  ;(sqrt tmmath-sqrt)
  ;(wide tmmath-wide)
  ;(neg tmmath-neg)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs->mathml env x)
  (set! tmmath-math-mode? #f)
  ((if (func? x '!file)
       tmmath-finalize-document
       tmmath-finalize-selection)
   (tmmath-root x)))
