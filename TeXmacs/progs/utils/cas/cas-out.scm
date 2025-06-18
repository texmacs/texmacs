
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cas-out.scm
;; DESCRIPTION : Conversion of content markup into presentation markup
;; TODO        : Parameterize by arbitrary grammar modules
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils cas cas-out)
  (:use (convert tools tmconcat) (utils cas cas-rewrite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to print special operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table cas-symbol-table
  (":=" . "<assign>")
  ("+=" . "<plusassign>")
  ("-=" . "<minusassign>")
  ("*=" . "<astassign>")
  ("/=" . "<overassign>")
  ("<<" . "<lflux>")
  (">>" . "<gflux>")
  ("|-" . "<vdash>")
  ("|=" . "<vDash>")
  ("=>" . "<Rightarrow>")
  ("<=>" . "<Leftrightarrow>")
  ("\\/" . "<vee>")
  ("/\\" . "<wedge>")
  ("!=" . "<neq>")
  ("<" . "<less>")
  ("<=" . "<leqslant>")
  (">" . "<gtr>")
  (">=" . "<geqslant>")
  ("->" . "<into>")
  (":->" . "<mapsto>")
  (":>" . "<transtype>")
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
;; Standard operators groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmsymbol->cas s)
  (let* ((l1 (ahash-table->list cas-symbol-table))
	 (l2 (map (lambda (x) (cons (cdr x) (car x))) l1))
	 (l3 (list-filter l2 (lambda (x) (== (car x) s))))
	 (l4 (map cdr l3)))
    (if (string-starts? s "<")
	(set! s (string-append "%" (substring s 1 (- (string-length s) 1)))))
    (map string->symbol (if (in? s l4) l4 (cons s l4)))))

(define (cas-collect-members . grs)
  (append-map
   (lambda (x) (append-map tmsymbol->cas (math-group-members x)))
   grs))

(define-macro (cas-define-test name . grs)
  (let* ((t (symbol-append name '-table))
	 (p (symbol-append name '?)))
    `(begin
       (define ,t (list->ahash-set (cas-collect-members ,@grs)))
       (define (,p x) (ahash-ref ,t x)))))

(cas-define-test cas-assign-op "arithmetic-assign")
(cas-define-test cas-flux-op "arithmetic-flux")
(cas-define-test cas-meta-op "logic-meta")
(cas-define-test cas-implies-op "logic-implication")
(cas-define-test cas-or-op "logic-disjunction")
(cas-define-test cas-and-op "logic-conjunction")
(cas-define-test cas-compare-op "logic-relation")
(cas-define-test cas-arrow-op "logic-arrow")
(cas-define-test cas-plus-op "arithmetic-plus" "arithmetic-set-symmetric")
(cas-define-test cas-minus-op "arithmetic-minus" "arithmetic-set-minus")
(cas-define-test cas-unary-minus-op "arithmetic-unary-minus")
(cas-define-test cas-times-op "arithmetic-times" "arithmetic-invisible-times")
(cas-define-test cas-over-op "arithmetic-over" "arithmetic-condensed-over")
(cas-define-test cas-prefix-op "logic-prefix" "arithmetic-prefix")

(define cas-special-op-table
  (ahash-table-append
    cas-assign-op-table
    cas-flux-op-table
    cas-meta-op-table
    cas-implies-op-table
    cas-or-op-table
    cas-and-op-table
    cas-compare-op-table
    cas-arrow-op-table
    cas-plus-op-table
    cas-minus-op-table
    cas-unary-minus-op-table
    cas-times-op-table
    cas-over-op-table
    cas-prefix-op-table
    (list->ahash-set '(%prime factorial ^ _ %dotaccess %sqaccess))
    (list->ahash-set '(quote quasiquote eval))
    (list->ahash-set '(matrix det bmatrix row tuple list set comma | ||))
    (list->ahash-set '(sqrt conj choose))))

(define (cas-special-op? x)
  (ahash-ref cas-special-op-table x))

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
  (or (cas-out-infix x cas-assign-op? cas-out-flux cas-out-flux)
      (cas-out-flux x)))

(define (cas-out-flux x)
  (or (cas-out-associative x cas-flux-op? cas-out-flux cas-out-meta)
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
  (or (cas-out-infix x cas-compare-op? cas-out-arrow cas-out-arrow)
      (cas-out-arrow x)))

(define (cas-out-arrow x)
  (or (cas-out-infix x cas-arrow-op? cas-out-plus cas-out-plus)
      (cas-out-plus x)))

(define (cas-out-plus x)
  (or (cas-out-associative x cas-plus-op? cas-out-plus cas-out-times)
      (cas-out-infix x cas-minus-op? cas-out-plus cas-out-times)
      (cas-out-times x)))

(define (cas-out-times x)
  (or (and (func? x '/ 1) `(frac "1" ,(cas-out (cadr x))))
      (and (func? x '/ 2) `(frac ,(cas-out (cadr x)) ,(cas-out (caddr x))))
      (and (func? x 'div 2)
	   `(concat ,(cas-out-times (cadr x)) " div "
		    ,(cas-out-prefix (caddr x))))
      (and (func? x 'mod 2)
	   `(concat ,(cas-out-times (cadr x)) " mod "
		    ,(cas-out-prefix (caddr x))))
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

(define (cas-out-decompose-prime x)
  (if (func? x '%prime 1)
      (receive (radical prime) (cas-out-decompose-prime (cadr x))
	(values radical (string-append "'" prime)))
      (values x "")))

(define (cas-out-postfix x)
  (cond ((func? x '%prime 1)
	 (receive (radical prime) (cas-out-decompose-prime x)
	   `(concat ,(cas-out-postfix radical) (rprime ,prime))))
	((func? x 'factorial 1)
	 `(concat ,(cas-out-postfix (cadr x)) "!"))
	((func? x '_)
	 (with radical (cas-out-postfix (cadr x))
	   (with args (cas-out-several (cddr x))
	     `(concat ,radical (rsub ,(apply tmconcat args))))))
	((and (func? x '^) (not (cas-is-root? x)))
	 (with radical (cas-out-postfix (cadr x))
	   (with args (cas-out-several (cddr x))
	     `(concat ,radical (rsup ,(apply tmconcat args))))))
	((func? x '%dotaccess 2)
	 `(concat ,(cas-out-postfix (cadr x)) "." ,(cas-out-atom (caddr x))))
	((func? x '%sqaccess)
	 (with radical (cas-out-postfix (cadr x))
	   (with args (cas-out-several (cddr x))
	     `(concat ,radical (left "[") ,@args (right "]")))))
	((list-1? x) `(concat ,(cas-out-postfix (car x)) "()"))
	((and (pair? x) (keyword? (car x)))
	 (cons (keyword->symbol (car x)) (map cas-out (cdr x))))
	((and (pair? x) (not (cas-special-op? (car x))))
	 ;; FIXME: also check arities
	 (with args (cas-out-several (cdr x))
	   `(concat ,(cas-out-postfix (car x)) (left "(") ,@args (right ")"))))
	(else (cas-out-atom x))))

(define (cas-out-cell x)
  `(cell ,(cas-out x)))

(define (cas-out-quasiquote x)
  (cond ((nlist? x) x)
	((func? x 'unquote 1) (cas-out (cadr x)))
	(else (map cas-out-quasiquote x))))

(define (cas-out-atom x)
  (cond ((null? x) "null")
	((number? x) (number->string x))
	((symbol? x) (cas->tmsymbol x))
	((string? x) x)
	((func? x 'quote 1) (cadr x))
	((func? x 'quasiquote 1) (cas-out-quasiquote (cadr x)))
	((func? x 'eval 1) (eval (cadr x)))
	((func? x 'eval 2)
	 (plugin-eval (cadr x) "default" (tm->tree (caddr x))))
	((func? x 'matrix) `(matrix (table ,@(map cas-out (cdr x)))))
	((func? x 'det) `(det (table ,@(map cas-out (cdr x)))))
	((func? x 'bmatrix) `(matrix (table ,@(map cas-out (cdr x)))))
	((func? x 'row) `(row ,@(map cas-out-cell (cdr x))))
	((== x '(tuple)) "()")
	((func? x 'tuple)
	 (with args (cas-out-several (cdr x))
	   `(concat (left "(") ,@args (right ")"))))
	((== x '(list)) "[]")
	((func? x 'list)
	 (with args (cas-out-several (cdr x))
	   `(concat (left "[") ,@args (right "]"))))
	((== x '(set)) "{}")
	((func? x 'set)
	 (with args (cas-out-several (cdr x))
	   `(concat (left "{") ,@args (right "}"))))
	((== x '(comma)) "null")
	((func? x 'comma)
	 (with args (cas-out-several (cdr x))
	   `(concat ,@args)))
	((cas-is-root? x)
	 (with y (cAr (caddr x))
	   (if (== y 2)
	       `(sqrt ,(cas-out (cadr x)))
	       `(sqrt ,(cas-out (cadr x)) ,(cas-out y)))))
	((func? x 'sqrt 1)
	 `(sqrt ,(cas-out (cadr x))))
	((func? x 'conj 1)
	 `(wide ,(cas-out (cadr x)) "<bar>"))
	((func? x 'choose 2)
	 `(choose ,(cas-out (cadr x)) ,(cas-out (caddr x))))
	((nlist? x) x)
	(else `(concat (left "(") ,@(cas-out-several (list x)) (right ")")))))

(define (cas-out-several l)
  (cond ((null? l) '())
	((list-1? l)
	 (with x (car l)
	   (cond ((func? x '|)
		  (cons* (cas-out (cadr x)) '(mid "|")
			 (cas-out-several (cddr x))))
		 ((func? x '||)
		  (cons* (cas-out (cadr x)) '(mid "||")
			 (cas-out-several (cddr x))))
		 (else (list (cas-out x))))))
	(else (cons* (cas-out (car l)) "," (cas-out-several (cdr l))))))

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
  (:synopsis "Convert mathematical expression @x into TeXmacs tree")
  (let* ((y1 (cas-normal-opposites x))
	 (y2 (cas-normal-inverses y1))
	 (y3 (cas-normal-associative y2 '+))
	 (y4 (cas-normal-associative y3 '*))
	 (y5 (cas-simplify-constants y4))
	 ;;(z1 (cas-sort y5))
	 (z1 (cas-polynomial-sort y5))
	 (z2 (cas-arrange-subtractions z1))
	 (z3 (cas-make-fractions z2))
	 (z4 (cas-make-binary z3 '+ '- 0))
	 (z5 (cas-make-binary z4 '+& '- 0))
	 (z6 (cas-make-binary z5 '* '/ 1))
	 (z7 (cas-make-binary z6 '*& '/ 1)))
    (cas-concat-simplify (cas-out z7))))
