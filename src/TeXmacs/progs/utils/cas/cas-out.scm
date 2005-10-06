
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cas-out.scm
;; DESCRIPTION : Conversion of content markup into presentation markup
;; TODO        : Parameterize by arbitrary grammar modules
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils cas cas-out)
  (:use (convert tools tmconcat) (utils cas cas-rewrite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to print symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table cas-symbol-table
  (":=" . "<assign>")
  ("+=" . "<plusassign>")
  ("-=" . "<minusassign>")
  ("*=" . "<astassign>")
  ("/=" . "<overassign>")
  ("|-" . "<vdash>")
  ("|=" . "<vDash>")
  ("=>" . "<Rightarrow>")
  ("<=>" . "<Leftrightarrow>")
  ("|" . "<vee>")
  ("&" . "<wedge>")
  ("!=" . "<neq>")
  ("<" . "<less>")
  ("<=" . "<leqslant>")
  (">" . "<gtr>")
  (">=" . "<geqslant>")
  ("+&" . "+")
  ("@+" . "<oplus>")
  ("*&" . "*")
  ("@*" . "<otimes>")
  ("!" . "<neg>"))

(define (cas->tmsymbol x)
  (with s (symbol->string x)
    (cond ((string-starts? s "%")
	   (string-append "<" (substring s 1 (string-length s)) ">"))
	  ((ahash-ref cas-symbol-table s) => identity)
	  (else s))))

(define (cas->umsymbol x)
  (with s (cas->tmsymbol x)
    (cond ((== s "-") "<um>")
	  ((== s "+") "<upl>")
	  ((== s "<pm>") "<upm>")
	  ((== s "<mp>") "<ump>")
	  (else s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-in-group? x g)
  (== (math-symbol-group (cas->tmsymbol x)) g))

(define (cas-assign-op? x)
  (cas-in-group? x "arithmetic-assign"))

(define (cas-meta-op? x)
  (cas-in-group? x "logic-meta"))

(define (cas-implies-op? x)
  (cas-in-group? x "logic-implication"))

(define (cas-or-op? x)
  (cas-in-group? x "logic-disjunction"))

(define (cas-and-op? x)
  (cas-in-group? x "logic-conjunction"))

(define (cas-compare-op? x)
  (cas-in-group? x "logic-relation"))

(define (cas-plus-op? x)
  (or (cas-in-group? x "arithmetic-plus")
      (cas-in-group? x "arithmetic-set-symmetric")))

(define (cas-minus-op? x)
  (or (cas-in-group? x "arithmetic-minus")
      (cas-in-group? x "arithmetic-set-minus")))

(define (cas-unary-minus-op? x)
  (cas-in-group? x "arithmetic-unary-minus"))

(define (cas-times-op? x)
  (or (cas-in-group? x "arithmetic-times")
      (cas-in-group? x "arithmetic-invisible-times")))

(define (cas-over-op? x)
  (or (cas-in-group? x "arithmetic-over")
      (cas-in-group? x "arithmetic-condensed-over")))

(define (cas-prefix-op? x)
  (or (cas-in-group? x "logic-prefix")
      (cas-in-group? x "arithmetic-prefix")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-out-infix x op? first second)
  (and (list-3? x) (op? (car x))
       `(concat ,(first (cadr x))
		,(cas->tmsymbol (car x))
		,(second (caddr x)))))

(define-table cas-related-table
  (+ + +& - %pm %mp)
  (+& + +& - %pm %mp)
  (%oplus %oplus %ominus)
  (* * *& /)
  (*& * *& /)
  (%otimes %otimes %oover))

(define (cas-out-associative x op? this next)
  (define (r y)
    (if (and (pair? y)
	     (with l (ahash-ref cas-related-table (car x))
	       (if l (in? (car y) l) (== (car y) (car x)))))
	(this y) (next y)))
  (and (pair? x) (nnull? (cdr x)) (op? (car x))
       (if (list-2? x) (r (cadr x))
	   `(concat ,(r (cadr x))
		    ,(cas->tmsymbol (car x))
		    ,(r (cons (car x) (cddr x)))))))

(define (cas-out x)
  (cas-out-assign x))

(define (cas-out-assign x)
  (or (cas-out-infix x cas-assign-op? cas-out-meta cas-out-meta)
      (cas-out-meta x)))

(define (cas-out-meta x)
  (or (cas-out-infix x cas-meta-op? cas-out-implies cas-out-implies)
      (cas-out-implies x)))

(define (cas-out-implies x)
  (or (cas-out-infix x cas-implies-op? cas-out-or cas-out-or)
      (cas-out-or x)))

(define (cas-out-or x)
  (or (cas-out-associative x cas-or-op? cas-out-or cas-out-and)
      (cas-out-and x)))

(define (cas-out-and x)
  (or (cas-out-associative x cas-and-op? cas-out-and cas-out-compare)
      (cas-out-compare x)))

(define (cas-out-compare x)
  (or (cas-out-infix x cas-compare-op? cas-out-plus cas-out-plus)
      (cas-out-plus x)))

(define (cas-out-plus x)
  (or (cas-out-associative x cas-plus-op? cas-out-plus cas-out-times)
      (cas-out-infix x cas-minus-op? cas-out-plus cas-out-times)
      (cas-out-times x)))

(define (cas-out-times x)
  (or (and (func? x '/ 1) `(frac "1" ,(cas-out (cadr x))))
      (and (func? x '/ 2) `(frac ,(cas-out (cadr x)) ,(cas-out (caddr x))))
      (cas-out-associative x cas-times-op? cas-out-times cas-out-prefix)
      (cas-out-infix x cas-over-op? cas-out-times cas-out-prefix)
      (cas-out-prefix x)))

(define (cas-out-prefix x)
  (or (and (list-2? x)
	   (or (cas-minus-op? (car x)) (cas-prefix-op? (car x)))
	   `(concat ,(cas->umsymbol (car x)) ,(cas-out-prefix (cadr x))))
      (cas-out-postfix x)))

(define (cas-is-root? x)
  (and (func? x '^ 2)
       (or (func? (caddr x) '/ 1)
	   (and (func? (caddr x) '/ 2) (== (cadr (caddr x)) 1)))))

(define (cas-special-op? x)
  (or (cas-assign-op? x)
      (cas-meta-op? x)
      (cas-implies-op? x)
      (cas-or-op? x)
      (cas-and-op? x)
      (cas-compare-op? x)
      (cas-plus-op? x)
      (cas-minus-op? x)
      (cas-times-op? x)
      (cas-over-op? x)
      (cas-prefix-op? x)
      (in? x '(%prime factorial ^ _))
      (in? x '(matrix det row tuple list set comma))))

(define (cas-out-postfix x)
  (cond ((func? x '%prime 1)
	 `(concat ,(cas-out-postfix (cadr x)) (rprime "'")))
	((func? x 'factorial 1)
	 `(concat ,(cas-out-postfix (cadr x)) "!"))
	((func? x '_ 2)
	 `(concat ,(cas-out-postfix (cadr x)) (rsub ,(cas-out (caddr x)))))
	((and (func? x '^ 2) (not (cas-is-root? x)))
	 `(concat ,(cas-out-postfix (cadr x)) (rsup ,(cas-out (caddr x)))))
	((list-1? x) `(concat ,(cas-out-postfix (car x)) "()"))
	((and (pair? x) (keyword? (car x)))
	 (cons (keyword->symbol (car x)) (map cas-out (cdr x))))
	((and (pair? x) (not (cas-special-op? (car x))))
	 ;; FIXME: also check arities
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat ,(cas-out-postfix (car x)) (left "(") ,@args (right ")"))))
	(else (cas-out-atom x))))

(define (cas-out-cell x)
  `(cell ,(cas-out x)))

(define (cas-out-atom x)
  (cond ((null? x) "null")
	((number? x) (number->string x))
	((symbol? x) (cas->tmsymbol x))
	((string? x) x)
	((func? x 'matrix) `(matrix (table ,@(map cas-out (cdr x)))))
	((func? x 'det) `(det (table ,@(map cas-out (cdr x)))))
	((func? x 'row) `(row ,@(map cas-out-cell (cdr x))))
	((== x '(tuple)) "()")
	((func? x 'tuple)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "(") ,@args (right ")"))))
	((== x '(list)) "[]")
	((func? x 'list)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "[") ,@args (right "]"))))
	((== x '(set)) "{}")
	((func? x 'set)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "{") ,@args (right "}"))))
	((== x '(comma)) "null")
	((func? x 'comma)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat ,@args)))
	((cas-is-root? x)
	 (with y (cAr (caddr x))
	   (if (== y 2)
	       `(sqrt ,(cas-out (cadr x)))
	       `(sqrt ,(cas-out (cadr x)) ,(cas-out y)))))
	((nlist? x) x)
	(else `(concat (left "(") ,(cas-out x) (right ")")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final simplifications on produced TeXmacs tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cas-concat-buffer '())

(define (cas-build-concat x)
  (cond ((func? x 'concat) (for-each cas-build-concat (cdr x)))
	(else (set! cas-concat-buffer (cons (cas-concat-simplify x)
					    cas-concat-buffer)))))

(define (cas-concat-simplify x)
  "Linear time concat simplification of @x"
  ;; This is needed because sums are written like (+ (+ (+ a b) c) d)
  ;; so serialization may become quadratic in time for naive algorithms
  (cond ((func? x 'concat)
	 (with-global cas-concat-buffer '()
	   (for-each cas-build-concat (cdr x))
	   (apply tmconcat* (tmconcat-simplify (reverse cas-concat-buffer)))))
	(else (cas-map cas-concat-simplify x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cas->stree x)
  (:synopsis "Convert mathematical expression @x into TeXmacs tree.")
  (let* ((y1 (cas-normal-opposites x))
	 (y2 (cas-normal-inverses y1))
	 (y3 (cas-normal-associative y2 '+))
	 (y4 (cas-normal-associative y3 '*))
	 ;;(y5 (cas-sort y4))
	 (y5 (cas-polynomial-sort y4))
	 (z1 (cas-simplify-constants y5))
	 (z2 (cas-arrange-subtractions z1))
	 (z3 (cas-make-fractions z2))
	 (z4 (cas-make-binary z3 '+ '- 0))
	 (z5 (cas-make-binary z4 '+& '- 0))
	 (z6 (cas-make-binary z5 '* '/ 1))
	 (z7 (cas-make-binary z6 '*& '/ 1)))
    (cas-concat-simplify (cas-out z7))))
