
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
	 (l3 (tmconcat-structure-brackets l2)))
    (tmmath (cons 'concat! l3))))

(define (tmmath-concat-item x)
  (if (string? x)
      (with type (math-symbol-type x)
	(cond ((string-number? x) `(m:mn ,x))
	      ((== type "unknown") `(m:mi ,(cork->utf8 x)))
	      ((== type "symbol") `(m:mi ,(cork->utf8 x)))
	      (else `(m:mo ,(cork->utf8 x)))))
      (tmmath x)))

(define (tmmath-concat! l)
  (with r (tmconcat-structure-scripts l)
    (cond ((null? r) "")
	  ((null? (cdr r)) (tmmath-concat-item (car r)))
	  (else `(m:mrow ,@(map tmmath-concat-item r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-group l)
  `(m:mrow ,(tmmath (car l))))

(define (tmmath-left l) `(m:mo (@ (form "prefix")) ,(cork->utf8 (car l))))
(define (tmmath-mid l) `(m:mo ,(cork->utf8 (car l))))
(define (tmmath-right l) `(m:mo (@ (form "postfix")) ,(cork->utf8 (car l))))
(define (tmmath-big l) `(m:mo ,(car l)))

(define (tmmath-lsub l) (tmmath-concat `((lsub ,(car l)))))
(define (tmmath-lsup l) (tmmath-concat `((lsup ,(car l)))))
(define (tmmath-rsub l) (tmmath-concat `((rsub ,(car l)))))
(define (tmmath-rsup l) (tmmath-concat `((rsup ,(car l)))))

(define (tmmath-rsub! l)
  `(m:msub ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-rsup! l)
  `(m:msup ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-rsubsup! l)
  `(m:msubsup ,(tmmath (car l)) ,(tmmath (cadr l)) ,(tmmath (caddr l))))

(define (tmmath-frac l)
  `(m:mfrac ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-sqrt l)
  (if (null? (cdr l))
      `(m:msqrt ,(tmmath (car l)))
      `(m:mroot ,(tmmath (car l)) ,(tmmath (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-with l)
  (tmmath (cAr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-dispatch htable l)
  (let ((x (drd-ref ,htable (car l))))
    (and (procedure? x)
	 (x (cdr l)))))

(define (tmmath x)
  (if (string? x)
      (tmmath-concat (list x))
      (or (tmmath-dispatch 'tmmath-primitives% x)
	  "?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmmath-primitives%
  (concat tmmath-concat)
  (concat! tmmath-concat!)
  (group tmmath-group)
  (left tmmath-left)
  (mid tmmath-mid)
  (right tmmath-right)
  (big tmmath-big)
  ;(lprime tmmath-id)
  ;(rprime tmmath-id)
  ;(below tmmath-below)
  ;(above tmmath-above)
  ;(lsub tmmath-sub)
  ;(lsup tmmath-sup)
  (rsub tmmath-rsub)
  (rsup tmmath-rsup)
  (rsub! tmmath-rsub!)
  (rsup! tmmath-rsup!)
  (rsubsup! tmmath-rsubsup!)
  (frac tmmath-frac)
  (sqrt tmmath-sqrt)
  ;(wide tmmath-wide)
  ;(neg tmmath-neg)

  (with tmmath-with)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs->mathml env x)
  (display-err* "x= " x "\n")
  (tmmath x))
