
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmath.scm
;; DESCRIPTION : conversion of TeXmacs trees into MathML trees
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml tmmath)
  (:use (convert tools tmconcat)
	(convert tools tmtable)
	(convert mathml mathml-drd)
	(convert rewrite tmtm-brackets)))

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
	      ((logic-ref tm->mathml-constant% x) => (lambda (y) `(m:mn ,y)))
	      ((logic-ref tm->mathml-operator% x) => (lambda (y) `(m:mo ,y)))
              ((and (string-starts? x "<up-") (== (string-length x) 6))
               `(m:mo ,(substring x 4 5)))
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

(define (tmmath-rigid l)
  `(m:mrow ,(tmmath (car l))))

(define (convert-around x)
  (with d (downgrade-brackets x)
    (tmmath-concat (if (pair? d) (cdr d) (list d)))))

(define (tmmath-around l)
  (convert-around (cons 'around l)))

(define (tmmath-around* l)
  (convert-around (cons 'around* l)))

(define (tmmath-big-around l)
  (convert-around (cons 'big-around l)))

(define (tmmath-large x)
  (with y (logic-ref tm->mathml-large% x)
    (if y y (cork->utf8 x))))

(define (tmmath-left l) `(m:mo (@ (form "prefix")) ,(tmmath-large (car l))))
(define (tmmath-mid l) `(m:mo ,(tmmath-large (car l))))
(define (tmmath-right l) `(m:mo (@ (form "postfix")) ,(tmmath-large (car l))))

(define (tmmath-big l)
  (cond ((== (car l) ".") "")
	((logic-ref tm->mathml-big% (car l)) => (lambda (y) `(m:mo ,y)))
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
  (with acc (or (logic-ref tm->mathml-wide% (cadr l)) "")
    `(m:mover ,(tmmath (car l)) (m:mo ,acc))))

(define (tmmath-wide* l)
  (with acc (or (logic-ref tm->mathml-wide% (cadr l)) "")
    `(m:munder ,(tmmath (car l)) (m:mo ,acc))))

(define (tmmath-long-arrow l)
  (let* ((a (car l))
         (above (if (>= (length l) 2) (cadr l) ""))
         (below (if (>= (length l) 3) (caddr l) "")))
    (when (and (string? a) (string-starts? a "<rubber-"))
      (set! a (string-append "<" (substring a 8 (string-length a)))))
    (let* ((mo (tmmath a))
           (mo* (if (func? mo 'm:mo)
                    `(m:mo (@ (stretchy "true")) ,@(cdr mo))
                    mo)))
      (cond ((and (!= above "") (== below ""))
             `(m:mover ,mo* ,(tmmath above)))
            ((and (== above "") (!= below ""))
             `(m:munder ,mo* ,(tmmath below)))
            (else
              `(m:munderover ,mo* ,(tmmath below) ,(tmmath above)))))))

(define (tmmath-above l)
  `(m:mover ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-below l)
  `(m:munder ,(tmmath (car l)) ,(tmmath (cadr l))))

(define (tmmath-neg l)
  `(m:menclose (@ (notation "updiagonalstrike")) ,(tmmath (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-make-attrs fun l)
  (with attrs (list-filter (map fun l) identity)
    (if (null? attrs) '() `((@ ,@attrs)))))

(define (tmmath-make-cell-attr x)
  (cond ((== x '("cell-halign" "l")) '(columnalign "left"))
	((== x '("cell-halign" "c")) '(columnalign "center"))
	((== x '("cell-halign" "r")) '(columnalign "right"))
	((== x '("cell-valign" "t")) '(rowalign "top"))
	((== x '("cell-valign" "c")) '(rowalign "center"))
	((== x '("cell-valign" "b")) '(rowalign "bottom"))
	((== x '("cell-valign" "B")) '(rowalign "baseline"))
	((== x '("cell-valign" "f")) '(rowalign "axis"))
	(else #f)))

(define (tmmath-make-cell c cellf)
  `(m:mtd ,@(tmmath-make-attrs tmmath-make-cell-attr cellf)
	  ,(tmmath (cadr c))))

(define (tmmath-make-cells l cellf)
  (if (null? l) l
      (cons (tmmath-make-cell (car l) (car cellf))
	    (tmmath-make-cells (cdr l) (cdr cellf)))))

(define (tmmath-make-row-attr x)
  (tmmath-make-cell-attr x))

(define (tmmath-make-row r rowf cellf)
  `(m:mtr ,@(tmmath-make-attrs tmmath-make-row-attr rowf)
	  ,@(tmmath-make-cells (cdr r) cellf)))

(define (tmmath-make-rows l rowf cellf)
  (if (null? l) l
      (cons (tmmath-make-row  (car l) (car rowf) (car cellf))
	    (tmmath-make-rows (cdr l) (cdr rowf) (cdr cellf)))))

(define (tmmath-make-column-attr l)
  (cond ((null? l) "left")
	((== (car l) '("cell-halign" "l")) "left")
	((== (car l) '("cell-halign" "c")) "center")
	((== (car l) '("cell-halign" "r")) "right")
	(else (tmmath-make-column-attr (cdr l)))))

(define (tmmath-make-table-attr x)
  (cond ((== x '("table-valign" "t")) '(align "top"))
	((== x '("table-valign" "c")) '(align "center"))
	((== x '("table-valign" "b")) '(align "bottom"))
	((== x '("table-valign" "B")) '(align "baseline"))
	((== x '("table-valign" "f")) '(align "axis"))
	(else (tmmath-make-cell-attr x))))

(define (tmmath-make-table t tablef colf rowf cellf)
  (let* ((l1 (list-filter (map tmmath-make-table-attr tablef) identity))
	 (l2 (map tmmath-make-column-attr (map reverse colf)))
	 (cs (apply string-append (list-intersperse l2 " ")))
	 (l3 (cons `(columnalign ,cs) l1)))
    `(m:mtable (@ ,@l3) ,@(tmmath-make-rows (cdr t) rowf cellf))))

(define (tmmath-table l)
  (list (tmmath-make-table (cons 'table l) '() '() '() '())))

(define (tmmath-tformat l)
  (with t (tmtable-normalize (cons 'tformat l))
    (receive (tablef colf rowf cellf) (tmtable-properties** t)
      (tmmath-make-table (cAr t) tablef colf rowf cellf))))

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

(define (tmmath-text x)
  ;; we protect via non-breaking spaces the initial and final whitespaces 
  ;; which are otherwise stripped by the MathML processor from <mtext> tags
  ;; see https://www.xmlmind.com/tutorials/MathML/
  (let* ((s (texmacs->code x  "utf-8"))
         (s (if (string-starts? s " ") 
            (string-append "&#xA0; " (substring s 1 (string-length s))) s))
         (s (if (string-ends? s " ") 
            (string-append (substring s 0 (- (string-length s) 1)) " &#xA0;") s)))
    `(m:mtext ,s)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmath-dispatch htable l)
  (let ((x (logic-ref ,htable (car l))))
    (and (procedure? x)
	 (x (cdr l)))))

(define (tmmath x)
  (if (!= (ahash-ref tmmath-env "mode") "math")
      (cond ((string? x) (tmmath-text x))
            ((== (car x) 'with) (tmmath-with (cdr x)))
            (else `(m:mrow ,@(map tmmath (cdr x)))))
      (cond ((string? x) (tmmath-concat (list x)))
            (else (or (tmmath-dispatch 'tmmath-primitives% x) "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-dispatcher tmmath-primitives%
  ;; Mathematics
  (concat tmmath-concat)
  (concat! tmmath-concat!)
  (rigid tmmath-rigid)
  (around tmmath-around)
  (around* tmmath-around*)
  (big-around tmmath-big-around)
  (left tmmath-left)
  (mid tmmath-mid)
  (right tmmath-right)
  (big tmmath-big)
  (long-arrow tmmath-long-arrow)
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
  (syntax tmmath-first)

  ;; Tabular markup
  (tformat tmmath-tformat)
  (table tmmath-table)
  (row tmmath-concat)
  (cell tmmath-concat)
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
