
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : regexp-match.scm
;; DESCRIPTION : pattern matching
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel regexp regexp-match)
  (:export
    define-grammar-decls ;; for define-grammar macro
    define-grammar
    match?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intersections and unions of solution sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bindings-add bl var val)
  "Bind variable @var to @val in @bl if possible."
  (cond ((assoc-ref bl var)
	 (if (== (assoc-ref bl var) val) bl #f))
	(else (cons (cons var val) bl))))

(define (match-any l pat bls)
  "Matches for @l == @pat under any of the bindings in @bls."
  (if (null? bls) '()
      (append (match l pat (car bls))
	      (match-any l pat (cdr bls)))))

(define-macro (match-cup expr1 expr2)
  `(let ((l1 ,expr1))
     (if (== l1 '(())) '(())
	 (let ((l2 ,expr2))
	   (if (== l2 '(())) '(())
	       (append l1 l2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (match-or l args pat bl)
  "Matches for @l == @((:or . args) . pat) under bindings @bl."
  (if (null? args) '()
      (match-cup (match l (cons (car args) pat) bl)
		 (match-or l (cdr args) pat bl))))

(define (match-and l args pat bl)
  "Matches for @l == @((:and . args) . pat) under bindings @bl."
  (if (null? args) (list bl)
      (match-any l (cons (car args) pat)
		 (match-and l (cdr args) pat bl))))

(define (match-not l args pat bl)
  "Matches for @l == @((:not . args) . pat) under bindings @bl."
  (if (or (null? l) (not (= (length args) 1))
	  (not (null? (match l (append args pat) bl)))) '()
      (match (cdr l) pat bl)))

(define (match-repeat l args pat bl)
  "Matches for @l == @((:repeat . args) . pat) under bindings @bl."
  (if (null? args) '()
      (match-cup (match l pat bl)
		 (match l (append args (cons (cons :repeat args) pat)) bl))))

(define (match-group l args pat bl)
  "Matches for @l == @((:group . args) . pat) under bindings @bl."
  (match l (append args pat) bl))

(define (match-quote l args pat bl)
  "Matches for @l == @((:quote . args) . pat) under bindings @bl."
  (if (or (null? l) (not (= (length args) 1))
	  (not (== (car l) (car args)))) '()
      (match (cdr l) pat bl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define match-term (make-ahash-table))
(define match-table (make-ahash-table))
(ahash-set! match-table :or match-or)
(ahash-set! match-table :and match-and)
(ahash-set! match-table :not match-not)
(ahash-set! match-table :repeat match-repeat)
(ahash-set! match-table :group match-group)
(ahash-set! match-table :quote match-quote)

(define (match l pat bl)
  "Matches for @l == @pat under bindings @bl."
  (if (null? pat) (if (== l '()) (list bl) '())
      (let ((fpat (car pat)))
	(cond ((keyword? fpat)
	       (let* ((symb (keyword->symbol fpat))
		      (n (string->number (symbol->string symb))))
		 (cond ((== fpat :*) (list bl))
		       (n (if (>= (length l) n)
			      (match (list-tail l n) (cdr pat) bl)
			      '()))
		       ((null? l) '())
		       ((ahash-ref match-term fpat)
			(match l
				(append (ahash-ref match-term fpat) (cdr pat))
				bl))
		       ((not (apply (eval symb) (list (car l)))) '())
		       (else (match (cdr l) (cdr pat) bl)))))
	      ((and (list? fpat) (not (null? fpat)))
	       (let ((ffpat (car fpat)))
		 (cond ((and (keyword? ffpat) (ahash-ref match-table ffpat))
			(apply (ahash-ref match-table ffpat)
			       (list l (cdr fpat) (cdr pat) bl)))
		       ((or (not (list? l)) (null? l)) '())
		       ((== ffpat 'quote)
			(let ((new-bl (bindings-add bl (cadr fpat) (car l))))
			  (if new-bl (match (cdr l) (cdr pat) new-bl) '())))
		       ((list? (car l))
			(match-any (cdr l) (cdr pat)
				   (match (car l) fpat bl)))
		       (else '()))))
	      ((null? l) '())
	      ((not (== (car l) fpat)) '())
	      (else (match (cdr l) (cdr pat) bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-grammar-decls l)
  (define (insert rule)
    (ahash-set! match-term (car rule) (cdr rule)))
  (for-each insert l))

(define-macro (define-grammar . l)
  `(begin
     (define-grammar-decls ,(list 'quasiquote l))))

(define (match? x pattern)
  "Does @x match the pattern @pat?"
  (let ((sols (match (list x) (list pattern) '())))
    (if (null? sols) #f sols)))
