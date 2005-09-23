
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
  (:use (convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting routines for putting expressions into operational forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-map fun x)
  "Map function to arguments of outermost function of @x"
  (cond ((list? x) (map fun x))
	(else x)))

(define (cas-normal-associative-sub x op)
  (cond ((and (pair? x) (== (car x) op))
	 (append-map (cut cas-normal-associative-sub <> op) (cdr x)))
	(else (list (cas-map (cut cas-normal-associative <> op) x)))))

(define (cas-normal-associative x op)
  "Makes all operations @op inside @x n-ary"
  (cond ((and (pair? x) (== (car x) op))
	 (cons op (append-map (cut cas-normal-associative-sub <> op) (cdr x))))
	(else (cas-map (cut cas-normal-associative <> op) x))))

(define (cas-opposite x)
  (cond ((== x 0) 0)
	((and (number? x) (> x 0)) (list '- x))
	((func? x '+) (cons '+ (map cas-opposite (cdr x))))
	((func? x '- 1) (cas-normal-opposites (cadr x)))
	((func? x '-)
	 (cons* '+ (cas-opposite (cadr x))
		   (map cas-normal-opposites (cddr x))))
	(else (with y (cas-normal-opposites x)
		(if (func? y '- 1) (cadr y) (list '- y))))))

(define (cas-normal-opposites x)
  "Turn all subtractions inside @x into unary opposites"
  (cond ((and (number? x) (< x 0)) (list '- (- x)))
	((func? x '- 0) 0)
	((func? x '- 1) (cas-opposite (cadr x)))
	((func? x '-) (cons* '+ (cadr x) (map cas-opposite (cddr x))))
	((or (func? x '*) (func? x '/))
	 (let* ((l1 (map cas-normal-opposites (cdr x)))
		(l2 (map (lambda (y) (if (func? y '- 1) (cadr y) y)) l1))
		(l3 (map (lambda (y) (if (func? y '- 1) -1 1)) l1)))
	   (if (== (apply * l3) 1)
	       (cons (car x) l2)
	       (list '- (cons (car x) l2)))))
	(else (cas-map cas-normal-opposites x))))

(define (cas-inverse-pow x flag?)
  (let ((what (cas-normal-inverses (cadr x)))
	(pow  (cas-normal-inverses (caddr x))))
    (when (func? what '/ 1)
      (set! what (cadr what))
      (set! flag? (not flag?)))
    (when (func? pow '- 1)
      (set! pow (cadr pow))
      (set! flag? (not flag?)))
    (with y (if (== pow 1) what (list '^ what pow))
      (if flag? (list '/ y) y))))

(define (cas-inverse x)
  (cond ((== x 1) 1)
	((func? x '*) (cons '* (map cas-inverse (cdr x))))
	((func? x '/ 1) (normalize (cadr x)))
	((func? x '/)
	 (cons* '* (cas-inverse (cadr x))
		   (map cas-normal-inverses (cddr x))))
	((func? x '^ 2) (cas-inverse-pow x #t))
	(else (with y (cas-normal-inverses x)
		(if (func? y '/ 1) (cadr y) (list '/ y))))))

(define (cas-normal-inverses x)
  "Turn all fractions inside @x into unary inverses"
  (cond ((func? x '/ 0) 1)
	((func? x '/ 1) (cas-inverse (cadr x)))
	((func? x '/)
	 (cons* '* (cas-normal-inverses (cadr x)) (map cas-inverse (cddr x))))
	((func? x '^ 2) (cas-inverse-pow x #f))
	(else (cas-map cas-normal-inverses x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting mathematical expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-size x)
  "Size of @x as an expression"
  (cond ((null? x) 0)
	((pair? x) (+ (cas-size (car x)) (cas-size (cdr x))))
	(else 1)))

(define (cas->number x)
  (cond ((number? x) x)
	((and (func? x '- 1) (cas->number (cadr x))) => -)
	(else #f)))

(define (number->cas x)
  (if (< x 0) (list '- (- x)) x))

(define (cas-sub<? x y flag?)
  (let* ((nx (cas->number x))
	 (ny (cas->number y))
	 (sx (cas-size x))
	 (sy (cas-size y)))	 
    (cond ((or nx ny)
	   (cond ((not ny) flag?)
		 ((not nx) (not flag?))
		 (else (< nx ny))))
	  ((< sx sy) flag?)
	  ((> sx sy) (not flag?))
	  ((or (symbol? x) (symbol? y))
	   (cond ((not (symbol? y)) #t)
		 ((not (symbol? x)) #f)
		 (else (string<? (symbol->string x) (symbol->string y)))))
	  ((or (string? x) (string? y))
	   (cond ((not (string? y)) #t)
		 ((not (string? x)) #f)
		 (else (string<? x y))))
	  ((or (pair? x) (pair? y))
	   (cond ((not (pair? y)) #t)
		 ((not (pair? x)) #f)
		 ((cas-sub<? (car x) (car y) flag?) #t)
		 ((cas-sub<? (car y) (car x) flag?) #f)
		 (else (cas-sub<? (cdr x) (cdr y) flag?))))
	  (else (and (null? x) (nnull? y))))))

(define (cas-default<=? x y)
  "Default lexicographical and total size ordering on expressions @x and @y"
  (or (cas-sub<? x y #t) (== x y)))

(define (cas-sum<=? x y)
  "Ordering for @x and @y in sums"
  (or (cas-sub<? x y #f) (== x y)))

(define (cas-product<=? x y)
  "Ordering for @x and @y in products"
  (cond ((or (cas->number x) (cas->number y))
	 (cond ((not (cas->number y)) #t)
	       ((not (cas->number x)) #f)
	       (else (< (cas->number x) (cas->number y)))))
	(else (cas-default<=? x y))))

(define (cas-sort x)
  "Sort the expression @x"
  (cond ((func? x '+) (cons '+ (sort (cdr x) cas-sum<=?)))
	((func? x '*) (cons '* (sort (cdr x) cas-product<=?)))
	(else (cas-map cas-sort x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-degree-max l)
  (if (null? l) -1
      (let ((d1 (car l))
	    (d2 (cas-degree-max (cdr l))))
	(if (cas-default<=? (cas-sort d1) (cas-sort d2)) d2 d1))))

(define (cas-degree-sum l)
  (if (null? l) 0
      (let* ((d1 (car l))
	     (d2 (cas-degree-sum (cdr l)))
	     (n1 (cas->number d1))
	     (n2 (cas->number d2))
	     (l1 (if (func? d1 '+) (cdr d1) (list d1)))
	     (l2 (if (func? d2 '+) (cdr d2) (list d2))))
	(cond ((== d1 0) d2)
	      ((== d2 0) d1)
	      ((and n1 n2) (cas->number (+ n1 n2)))
	      (else (cons '+ (append l1 l2)))))))

(define (cas-degree-opposite d)
  (cas-opposite d))

(define (cas-degree x var)
  "Determine the degree of @x in @var"
  (cond ((or (func? x '-) (func? x '+))
	 (cas-degree-max (map (cut cas-degree <> var) (cdr x))))
	((func? x '*)
	 (cas-degree-sum (map (cut cas-degree <> var) (cdr x))))
	((func? x '/ 1)
	 (cas-degree-opposite (cas-degree (cadr x) var)))
	((func? x '/)
	 (cas-degree (cons '* (cadr x) (map cas-inverse (cddr x)))))
	((and (func? x '^) (== (cadr x) var)) (caddr x))
	((== x var) 1)
	(else 0)))

(define (cas-term<=? x y vars)
  "Order @x and @y accordings degrees in variables in the list @vars"
  (if (null? vars) (cas-sum<=? x y)
      (let* ((dx (cas-degree x (car vars)))
	     (dy (cas-degree y (car vars))))
	(if (== dx dy)
	    (cas-term<=? x y (cdr vars))
	    (cas-default<=? dy dx)))))

(define (cas-radicals x)
  "Get all 'polynomial' variables of @x"
  (cond ((and (pair? x) (in? (car x) '(+ - * /)))
	 (append-map cas-radicals (cdr x)))
	((func? x '^ 2) (list (cadr x)))
	(else '())))

(define (cas-polynomial-sort x)
  "Recursively sort the expression @x as a polynomial"
  (cond ((func? x '+)
	 (with vars (reverse (sort (cas-radicals x) cas-product<=?))
	   (cons '+ (sort (cdr x) (cut cas-term<=? <> <> vars)))))
	((func? x '*) (cons '* (sort (cdr x) cas-product<=?)))
	(else (cas-map cas-polynomial-sort x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification of constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-simplify-constants-sub l op)
  (cond ((null? l) l)
	((== (car l) 0) (cas-simplify-constants-sub (cdr l) op))
	((null? (cdr l)) l)
	((and (cas->number (car l)) (cas->number (cadr l)))
	 (cas-simplify-constants-sub
	  (cons (op (cas->number (car l)) (cas->number (cadr l)))
		(cddr l)) op))
	(else (cons (car l) (cas-simplify-constants-sub (cdr l) op)))))

(define (cas-simplify-constants x)
  "Simplify constants in @x"
  (cond ((func? x '+)
	 (with l (cas-simplify-constants-sub (cdr x) +)
	   (cond ((null? l) 0)
		 ((null? (cdr l)) (car l))
		 (else (cons '+ l)))))
	((func? x '*)
	 (with l (cas-simplify-constants-sub (cdr x) *)
	   (cond ((null? l) 1)
		 ((null? (cdr l)) (car l))
		 (else (cons '* l)))))
	((func? x '- 1)
	 (let* ((y (cas-simplify-constants (cadr x)))
		(n (cas->number y)))
	   (if n (number->cas (- n)) (list '- y))))
	((func? x '/ 1)
	 (let* ((y (cas-simplify-constants (cadr x)))
		(n (cas->number y)))
	   (if (in? n '(1 -1)) y (list '/ y))))
	((func? x '^ 2)
	 (let* ((y1 (cas-simplify-constants (cadr x)))
		(y2 (cas-simplify-constants (caddr x)))
		(n1 (cas->number y1))
		(n2 (cas->number y2)))
	   (cond ((== n1 0) 0)
		 ((== n1 1) 1)
		 ((== n2 0) 1)
		 ((== n2 1) y1)
		 (else (list '^ y1 y2)))))
	(else (cas-map cas-simplify-constants x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting routines for putting an expression into a printable form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-arrange-subtractions x)
  "Avoid sums which start with unary minus when possible"
  (cond ((and (func? x '+) (> (length x) 2) (< (length x) 6)
	      (func? (cadr x) '- 1))
	 (with l (map cas-arrange-subtractions (cdr x))
	   (with i (list-find-index l (lambda (y) (not (func? y '- 1))))
	     (if (not i) (cas-map cas-arrange-subtractions x)
		 `(+ ,(list-ref l i)
		     ,@(list-head l i)
		     ,@(list-tail l (+ i 1)))))))
	(else (cas-map cas-arrange-subtractions x))))

(define (cas-make-fractions x)
  "Turn n-ary multiplications with unary inverses inside @x into fractions"
  (cond ((func? x '/ 1) (list '/ 1 (cas-make-fractions (cadr x))))
	((func? x '*)
	 (receive (dl nl) (list-partition (cdr x) (lambda (y) (func? y '/ 1)))
	   (let* ((nl* (map cas-make-fractions nl))
		  (dl* (map cas-make-fractions (map cadr dl)))
		  (num (if (null? nl*) 1 (cons '* nl*)))
		  (den (if (null? dl*) 1 (cons '* dl*))))
	     (if (== den 1) num (list '/ num den)))))
	(else (cas-map cas-make-fractions x))))

(define (cas-make-binary-sub l op inv neu)
  (cond ((null? l) neu)
	((null? (cdr l)) (car l))
	((func? (cadr l) inv 1)
	 (cas-make-binary-sub
	  (cons (list inv (car l) (cadadr l)) (cddr l)) op inv neu))
	(else
	 (cas-make-binary-sub
	  (cons (list op (car l) (cadr l)) (cddr l)) op inv neu))))

(define (cas-make-binary x op inv neu)
  "Make n-ary operations @op and unary inverses @inv inside @x binary"
  (with make-binary (cut cas-make-binary <> op inv neu)
    (cond ((and (pair? x) (== (car x) op))
	   (cas-make-binary-sub (map make-binary (cdr x)) op inv neu))
	  (else (cas-map make-binary x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-out x)
  (cas-out-formula x))

(define (cas-out-formula x)
  (cond ((func? x '=> 2)
	 `(concat ,(cas-out-or (cadr x)) "<Rightarrow>"
		  ,(cas-out-or (caddr x))))
	((func? x '<=> 2)
	 `(concat ,(cas-out-or (cadr x)) "<Leftrightarrow>"
		  ,(cas-out-or (caddr x))))
	(else (cas-out-or x))))

(define (cas-out-or x)
  (cond ((func? x '| 2)
	 `(concat ,(cas-out-or (cadr x)) "<vee>" ,(cas-out-or (caddr x))))
	(else (cas-out-and x))))

(define (cas-out-and x)
  (cond ((func? x '& 2)
	 `(concat ,(cas-out-and (cadr x)) "<wedge>" ,(cas-out-and (caddr x))))
	(else (cas-out-compare x))))

(define (cas-out-compare x)
  (cond ((func? x '= 2)
	 `(concat ,(cas-out-plus (cadr x)) "=" ,(cas-out-plus (caddr x))))
	((func? x '!= 2)
	 `(concat ,(cas-out-plus (cadr x)) "<neq>" ,(cas-out-plus (caddr x))))
	((func? x '< 2)
	 `(concat ,(cas-out-plus (cadr x)) "<less>" ,(cas-out-plus (caddr x))))
	((func? x '> 2)
	 `(concat ,(cas-out-plus (cadr x)) "<gtr>" ,(cas-out-plus (caddr x))))
	((func? x '<= 2)
	 `(concat ,(cas-out-plus (cadr x)) "<leqslant>"
		  ,(cas-out-plus (caddr x))))
	((func? x '>= 2)
	 `(concat ,(cas-out-plus (cadr x)) "<geqslant>"
		  ,(cas-out-plus (caddr x))))
	(else (cas-out-plus x))))

(define (cas-out-plus x)
  (cond ((func? x '+ 2)
	 `(concat ,(cas-out-plus (cadr x)) "+" ,(cas-out-plus (caddr x))))
	((func? x '- 2)
	 `(concat ,(cas-out-plus (cadr x)) "-" ,(cas-out-times (caddr x))))
	(else (cas-out-times x))))

(define (cas-out-times x)
  (cond ((func? x '*)
	 `(concat ,(cas-out-times (cadr x)) "*" ,(cas-out-times (caddr x))))
	((func? x '/ 1)
	 `(frac "1" ,(cas-out (cadr x))))
	((func? x '/ 2)
	 `(frac ,(cas-out (cadr x)) ,(cas-out (caddr x))))
	(else (cas-out-prefix x))))

(define (cas-out-prefix x)
  (cond ((func? x '- 1)
	 `(concat "<um>" ,(cas-out-prefix (cadr x))))
	((func? x '! 1)
	 `(concat "<neg>",(cas-out-prefix (cadr x))))
	(else (cas-out-postfix x))))

(define (cas-is-root? x)
  (and (func? x '^ 2)
       (or (func? (caddr x) '/ 1)
	   (and (func? (caddr x) '/ 2) (== (cadr (caddr x)) 1)))))

(define cas-out-special
  '(=> <=> & |
    = != < > <= >=
    + - * / ^ !
    %prime factorial _
    matrix det row tuple list set))

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
	((and (pair? x) (nin? (car x) cas-out-special))
	 ;; FIXME: also check arities
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat ,(cas-out-postfix (car x)) (left "(") ,@args (right ")"))))
	(else (cas-out-atom x))))

(define (cas-out-cell x)
  `(cell ,(cas-out x)))

(define (cas-out-atom x)
  (cond ((null? x) "null")
	((number? x) (number->string x))
	((symbol? x)
	 (with s (symbol->string x)
	   (if (not (string-starts? s "%")) s
	       (string-append "<" (substring s 1 (string-length s)) ">"))))
	((string? x) (string-append "\"" (escape-quotes x) "\""))
	((func? x 'matrix) `(matrix (table ,@(map cas-out (cdr x)))))
	((func? x 'det) `(det (table ,@(map cas-out (cdr x)))))
	((func? x 'row) `(row ,@(map cas-out-cell (cdr x))))
	((== x '(tuple)) "()")
	((func? x 'tuple)
	 (with args (tuple-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "(") ,@args (right ")"))))
	((== x '(list)) "[]")
	((func? x 'list)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "[") ,@args (right "]"))))
	((== x '(set)) "{}")
	((func? x 'set)
	 (with args (list-intersperse (map cas-out (cdr x)) ",")
	   `(concat (left "{") ,@args (right "}"))))
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
	 (y5 (cas-polynomial-sort y4))
	 (z1 (cas-simplify-constants y5))
	 (z2 (cas-arrange-subtractions z1))
	 (z3 (cas-make-fractions z2))
	 (z4 (cas-make-binary z3 '+ '- 0))
	 (z5 (cas-make-binary z4 '* '/ 1)))
    (cas-concat-simplify (cas-out z5))))
