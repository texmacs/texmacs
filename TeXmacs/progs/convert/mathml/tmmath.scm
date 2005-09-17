
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
  (:use (convert tools tmconcat)
	(convert mathml mathml-drd)))

(define tmmath-env (make-ahash-table))

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

(define (cork->utf8* x)
  (with y (cork->utf8 x)
    (if (and (== x y) (== (string-ref y 0) #\<)) "?" y)))

(define (tmmath-concat-item x)
  (if (string? x)
      (with type (math-symbol-type x)
	(cond ((string-number? x) `(m:mn ,x))
	      ((drd-ref tm->mathml-constant% x) => (lambda (y) `(m:mn ,y)))
	      ((drd-ref tm->mathml-operator% x) => (lambda (y) `(m:mo ,y)))
	      ((in? type '("unknown" "symbol")) `(m:mi ,(cork->utf8* x)))
	      (else `(m:mo ,(cork->utf8* x)))))
      (tmmath x)))

(define (tmmath-concat! l)
  (with r (map tmmath-concat-item (tmconcat-structure-scripts l))
    (set! r (list-filter r (lambda (x) (!= x ""))))
    (cond ((null? r) '(m:mrow))
	  ((null? (cdr r)) (car r))
	  (else `(m:mrow ,@r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-group l)
  `(m:mrow ,(tmmath (car l))))

(define (tmmath-large x)
  (with y (drd-ref tm->mathml-large% x)
    (if y y (cork->utf8 x))))

(define (tmmath-left l) `(m:mo (@ (form "prefix")) ,(tmmath-large (car l))))
(define (tmmath-mid l) `(m:mo ,(tmmath-large (car l))))
(define (tmmath-right l) `(m:mo (@ (form "postfix")) ,(tmmath-large (car l))))

(define (tmmath-big l)
  (cond ((== (car l) ".") "")
	((drd-ref tm->mathml-big% (car l)) => (lambda (y) `(m:mo ,y)))
	(else `(m:mo ,(car l)))))

(define (tmmath-lsub l) (tmmath-concat `((lsub ,(car l)))))
(define (tmmath-lsup l) (tmmath-concat `((lsup ,(car l)))))
(define (tmmath-rsub l) (tmmath-concat `((rsub ,(car l)))))
(define (tmmath-rsup l) (tmmath-concat `((rsup ,(car l)))))

(define (tmmath-lscript base sub sup)
  (if (and (pair? base) (in? (car base) '(m:msub m:msup m:msubsup)))
      (let ((nbase (cadr base))
	    (rsub '(m:none))
	    (rsup '(m:none)))
	(if (func? base 'm:msub) (set! rsub (caddr base)))
	(if (func? base 'm:msup) (set! rsup (caddr base)))
	(if (func? base 'm:msubsup) (set! rsub (caddr base)))
	(if (func? base 'm:msubsup) (set! rsup (cadddr base)))
	`(m:mmultiscripts ,nbase ,rsub ,rsup (m:mprescripts) ,sub ,sup))
      `(m:mmultiscripts ,base (m:mprescripts) ,sub ,sup)))

(define (tmmath-lsub! l)
  (tmmath-lscript (tmmath (car l)) (tmmath (cadr l)) '(m:none)))

(define (tmmath-lsup! l)
  (tmmath-lscript (tmmath (car l)) '(m:none) (tmmath (cadr l))))

(define (tmmath-lsubsup! l)
  (tmmath-lscript (tmmath (car l)) (tmmath (cadr l)) (tmmath (caddr l))))

(define (tmmath-with-limits? x)
  (and (func? x 'big)
       (== (ahash-ref tmmath-env "math-display") "true")))

(define (tmmath-rsub! l)
  (with op (if (tmmath-with-limits? (car l)) 'm:munder 'm:msub)
    (list op (tmmath (car l)) (tmmath (cadr l)))))

(define (tmmath-rsup! l)
  (with op (if (tmmath-with-limits? (car l)) 'm:mover 'm:msup)
    (list op (tmmath (car l)) (tmmath (cadr l)))))

(define (tmmath-rsubsup! l)
  (with op (if (tmmath-with-limits? (car l)) 'm:munderover 'm:msubsup)
    (list op (tmmath (car l)) (tmmath (cadr l)) (tmmath (caddr l)))))

(define (tmmath-frac l)
  `(m:mfrac ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-sqrt l)
  (if (null? (cdr l))
      `(m:msqrt ,(tmmath (car l)))
      `(m:mroot ,(tmmath (car l)) ,(tmmath (cadr l)))))

(define (tmmath-wide l)
  (with acc (or (drd-ref tm->mathml-wide% (cadr l)) "")
    `(m:mover ,(tmmath (car l)) (m:mo ,acc))))

(define (tmmath-wide* l)
  (with acc (or (drd-ref tm->mathml-wide% (cadr l)) "")
    `(m:munder ,(tmmath (car l)) (m:mo ,acc))))

(define (tmmath-above l)
  `(m:mover ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-below l)
  `(m:munder ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-neg l)
  `(m:menclose (@ (notation "updiagonalstrike")) ,(tmmath (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-cell l)
  `(m:mtd ,(tmmath (car l))))

(define (tmmath-row l)
  `(m:mtr ,@(map tmmath l)))

(define (tmmath-table l)
  `(m:mtable ,@(map tmmath l)))

(define (tmmath-tformat-rcl? l)
  (and (in? '(cwith "1" "-1" "-1" "-1" "cell-halign" "l") l)
       (in? '(cwith "1" "-1" "1" "1" "cell-halign" "r") l)))

(define (tmmath-tformat l)
  ;; FIXME: dirty hack to recognize at least eqnarrays
  ;; should be improved later
  (if (tmmath-tformat-rcl? (cDr l))
      (with x (cAr l)
	(while (or (func? x 'document 1) (func? x 'tformat))
	  (set! x (cAr x)))
	(if (func? x 'table)
	    `(m:mtable (@ (columnalign "right center left"))
		       ,@(map tmmath (cdr x)))
	    (tmmath x)))
      (tmmath (cAr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-noop l) "")

(define (tmmath-first l)
  (tmmath (car l)))

(define (tmmath-last l)
  (tmmath (cAr l)))

(define (tmmath-surround l)
  (tmmath-concat (list (car l) (caddr l) (cadr l))))

(define (tmmath-attr x)
  (with (var val) x
    (cond ((== var "color") (list 'mathcolor val))
	  ((== x '("math-font-series" "medium")) (list 'mathvariant "normal"))
	  ((== x '("math-font-series" "bold")) (list 'mathvariant "bold"))
	  ((== x '("math-font-series" "bold")) (list 'mathvariant "bold"))
	  ((== var "math-level") (list 'scriptlevel val))
	  ((== var "math-display") (list 'displaystyle val))
	  (else #f))))

(define (list-two-by-two l)
  (if (null? l) l
      (cons (list (car l) (cadr l)) (list-two-by-two (cddr l)))))

(define (tmmath-with-sub attrs body)
  (if (null? attrs) (tmmath body)
      (with (var val) (car attrs)
	(ahash-with tmmath-env var val
	  (tmmath-with-sub (cdr attrs) body)))))

(define (tmmath-with l)
  (let* ((attrs-1 (list-two-by-two (cDr l)))
	 (attrs-2 (map tmmath-attr attrs-1))
	 (attrs-3 (list-filter attrs-2 identity))
	 (body (tmmath-with-sub attrs-1 (cAr l))))
    (if (null? attrs-3) body
	`(m:mstyle (@ ,@attrs-3) ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-dispatch htable l)
  (let ((x (drd-ref ,htable (car l))))
    (and (procedure? x)
	 (x (cdr l)))))

(define (tmmath x)
  (cond ((!= (ahash-ref tmmath-env "mode") "math")
	 `(m:mtext ,(cork->utf8 (texmacs->verbatim (tm->tree x)))))
	((string? x) (tmmath-concat (list x)))
	(else (or (tmmath-dispatch 'tmmath-primitives% x) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmmath-primitives%
  ;; Mathematics
  (concat tmmath-concat)
  (concat! tmmath-concat!)
  (group tmmath-group)
  (left tmmath-left)
  (mid tmmath-mid)
  (right tmmath-right)
  (big tmmath-big)
  (lprime tmmath-lsup)
  (rprime tmmath-rsup)
  (below tmmath-below)
  (above tmmath-above)
  (lsub tmmath-lsub)
  (lsup tmmath-lsup)
  (rsub tmmath-rsub)
  (rsup tmmath-rsup)
  (lsub! tmmath-lsub!)
  (lsup! tmmath-lsup!)
  (lsubsup! tmmath-lsubsup!)
  (rsub! tmmath-rsub!)
  (rsup! tmmath-rsup!)
  (rsubsup! tmmath-rsubsup!)
  (frac tmmath-frac)
  (sqrt tmmath-sqrt)
  (wide tmmath-wide)
  (wide* tmmath-wide*)
  (neg tmmath-neg)
  (tree tmmath-noop)

  ;; Tabular markup
  (tformat tmmath-tformat)
  (table tmmath-table)
  (row tmmath-row)
  (cell tmmath-cell)
  (tabular tmmath-first)
  (tabular* tmmath-first)
  (block tmmath-first)
  (block* tmmath-first)

  ;; Other markup
  (document tmmath-concat)
  (para tmmath-concat)
  (surround tmmath-surround)
  (move tmmath-first)
  (resize tmmath-first)
  (with tmmath-with))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->mathml x . opt-env)
  (if (nnull? opt-env) (set! tmmath-env (car opt-env)))
  (ahash-with tmmath-env "mode" "math"
    (tmmath x)))

;(display-err* "x= " x "\n")
;(with y (tmmath x)
;(display-err* "y= " y "\n")
;y)))
