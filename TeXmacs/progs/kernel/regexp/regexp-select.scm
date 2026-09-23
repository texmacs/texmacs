
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : regexp-select.scm
;; DESCRIPTION : selection via patterns
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module provides a powerful routine for selecting subexpressions from
;; scheme trees and trees. It combines regular expression matching on paths
;; as well as nodes on the paths. It therefore provides for an analogue of
;; Xpath (except for non local paths, which will be considered later),
;; combined with usual pattern matching, and the extensibility of Scheme.
;; In the future, we also plan to use this facility for search & replace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel regexp regexp-select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intersections of matches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bindings-merge l1 l2)
  (if (null? l2) l1
      (with r1 (bindings-add l1 (caar l2) (cdar l2))
	(if r1 (bindings-merge r1 (cdr l2)) #f))))

(define (bindings-subst x bl)
  (cond ((npair? x) x)
	((func? x 'quote 1)
	 (with val (assoc-ref bl (cadr x))
	   (if val val x)))
	(else (cons (bindings-subst (car x) bl)
		    (bindings-subst (cdr x) bl)))))

(define (select-cap x1 x2)
  (if (!= (car x1) (car x2)) '()
      (with bl (bindings-merge (cddr x1) (cddr x2))
	(if bl (list (cons* (car x1) (cadr x1) bl)) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-symbol l nr sym bl)
  "Select symbols"
  (cond ((null? l) l)
	((and (tm-compound? (car l)) (== (tm-car (car l)) sym))
	 (cons (cons* (list nr) (car l) bl)
	       (select-symbol (cdr l) (+ nr 1) sym bl)))
	(else (select-symbol (cdr l) (+ nr 1) sym bl))))

(define (select-exclude-symbols l nr syms bl)
  "Select all except symbols in a given list"
  (cond ((null? l) l)
	((and (tm-compound? (car l)) (nin? (tm-car (car l)) syms))
	 (cons (cons* (list nr) (car l) bl)
	       (select-exclude-symbols (cdr l) (+ nr 1) syms bl)))
	(else (select-exclude-symbols (cdr l) (+ nr 1) syms bl))))

(define (select-exclude x args bl)
  "Select :exclude patterns"
  (select-exclude-symbols (cdr (tm->list x)) 0 args bl))

(define (select-range l nr begin end bl)
  "Select ranges"
  (cond ((or (null? l) (>= nr end)) '())
	((< nr begin) (select-range (cdr l) (+ nr 1) begin end bl))
	(else (cons (cons* (list nr) (car l) bl)
		    (select-range (cdr l) (+ nr 1) begin end bl)))))

(define (select-range* x args bl)
  "Select :range patterns"
  (if (and (tm-compound? x) (= (length args) 2))
      (select-range (cdr (tm->list x)) 0 (car args) (cadr args) bl)
      '()))

(define (select-or x args bl)
  "Select :or patterns"
  (if (null? args) '()
      (append (select-one x (car args) bl)
	      (select-or x (cdr args) bl))))

(define (select-and x args bl)
  "Select :and patterns"
  (cond ((null? (cdr args)) (select-one x (car args) bl))
	(else
	 (let* ((l1 (select-one x (car args) bl))
		(l2 (select-and x (cdr args) bl))
		(f1 (lambda (x1)
		      (with f2 (lambda (x2) (select-cap x1 x2))
			(append-map f2 l2)))))
	   (append-map f1 l1)))))

(define (select-and-not x args bl)
  "Select :and-not patterns"
  (let* ((l1 (select-one x (car args) bl))
	 (l2 (select-or x (cdr args) bl))
	 (f1 (lambda (x1)
	       (let f2 ((l2 l2))
		 (cond ((null? l2) (list x1))
		       ((null? (select-cap x1 (car l2))) (f2 (cdr l2)))
		       (else '()))))))
    (append-map f1 l1)))

(define (select-group x args bl)
  "Select :group patterns"
  (select-list x args bl))

(define (select-match x args bl)
  "Select :match patterns"
  (let* ((sols (match (list x) args '()))
	 (fun (lambda (sol-bl) (cons* (list) x sol-bl))))
    (map fun sols)))

(define (select-replace x args bl)
  "Replace selected pattern"
  (with r (bindings-subst (car args) bl)
    (list (cons* (list (list ':replace r)) r bl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (navigate-same p pat bl)
  (if (and p (nnull? p))
      (select-list (path->tree p) pat bl)
      '()))

(define (navigate-up p pat bl)
  (if (and p (nnull? p))
      (select-list (path->tree (cDr p)) pat bl)
      '()))

(define (navigate-down p pat bl)
  (with q (cDr (cursor-path))
    (if (and p (list-starts? (cDr q) p))
	(select-list (path->tree (list-head q (1+ (length p)))) pat bl)
	'())))

(define (navigate-first p pat bl)
  (if p
      (let* ((t (path->tree p))
	     (a (tree-arity t)))
	(if (> a 0)
	    (select-list (tree-ref t 0) pat bl)
	    '()))
      '()))

(define (navigate-last p pat bl)
  (if p
      (let* ((t (path->tree p))
	     (a (tree-arity t)))
	(if (> a 0)
	    (select-list (tree-ref t (- a 1)) pat bl)
	    '()))
      '()))

(define (navigate-next p pat bl)
  (if (and p (nnull? p))
      (let* ((t (path->tree (cDr p)))
	     (l (cAr p)))
	(if (< l (- (tree-arity t) 1))
	    (select-list (tree-ref t (+ l 1)) pat bl)
	    '()))
      '()))

(define (navigate-previous p pat bl)
  (if (and p (nnull? p))
      (let* ((t (path->tree (cDr p)))
	     (l (cAr p)))
	(if (> l 0)
	    (select-list (tree-ref t (- l 1)) pat bl)
	    '()))
      '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selecting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define select-table (make-ahash-table))
(ahash-set! select-table :exclude select-exclude)
(ahash-set! select-table :range select-range*)
(ahash-set! select-table :or select-or)
(ahash-set! select-table :and select-and)
(ahash-set! select-table :and-not select-and-not)
(ahash-set! select-table :group select-group)
(ahash-set! select-table :match select-match)
(ahash-set! select-table :replace select-replace)

(define navigate-table (make-ahash-table))
(ahash-set! navigate-table :same navigate-same)
(ahash-set! navigate-table :up navigate-up)
(ahash-set! navigate-table :down navigate-down)
(ahash-set! navigate-table :first navigate-first)
(ahash-set! navigate-table :last navigate-last)
(ahash-set! navigate-table :next navigate-next)
(ahash-set! navigate-table :previous navigate-previous)

(define (select-one x pat bl)
  (cond  ((pair? pat)
	  (with fpat (car pat)
	   (cond ((and (keyword? fpat) (ahash-ref select-table fpat))
		  (apply (ahash-ref select-table fpat)
			 (list x (cdr pat) bl)))
		 ((and (== fpat 'quote) (= (length pat) 2))
		  (with new-bl (bindings-add bl (cadr pat) x)
		    (if new-bl (list (cons* (list) x new-bl)) '())))
		 (else '()))))
	 ((and (keyword? pat) (!= pat :%1)) (select-list x (list pat) bl))
	 ((not (tm-compound? x)) '())
	 ((symbol? pat) (select-symbol (cdr (tm->list x)) 0 pat bl))
	 ((integer? pat) (select-range (cdr (tm->list x)) 0 pat (+ pat 1) bl))
	 ((== pat :%1) (select-range (cdr (tm->list x)) 0 0 (tm-arity x) bl))
	 (else '())))

(define (select-continue-sub x pat)
  (let* ((l (select-list (cadr x) pat (cddr x)))
	 (fun (lambda (r) (cons (append (car x) (car r)) (cdr r)))))
    (map fun l)))

(define (select-continue l pat)
  (with fun (lambda (r) (select-continue-sub r pat))
    (append-map fun l)))

(define (select-list x pat bl)
  ;; (display* "select list " x ", " pat ", " bl "\n")
  "Selects subexpressions of @l using the pattern @pat and under bindings @bl."
  (cond ((null? pat) (list (cons* (list) x bl)))
	((npair? pat) '())
	((keyword? (car pat))
	 (with fpat (car pat)
	   (cond ((== fpat :%0)
		  (select-list x (cdr pat) bl))
		 ((== fpat :*)
		  (let* ((r (select-list x (cdr pat) bl))
			 (l (select-one x :%1 bl)))
		    (append r (select-continue l pat))))
		 ((keyword->number fpat)
		  (let* ((h (number->keyword (- (keyword->number fpat) 1)))
			 (l (select-one x :%1 bl)))
		    (select-continue l (cons h (cdr pat)))))
		 ((ahash-ref navigate-table fpat)
                  (with p (and (tree? x) (tree-get-path x))
                    (cond (p ((ahash-ref navigate-table fpat) p (cdr pat) bl))
                          ((and (in? fpat (list :first :last))
                                (tm-compound? x) (>= (tm-arity x) 1))
                           (with c (if (== fpat :first)
                                       (tm-ref x 0)
                                       (tm-ref x (- (tm-arity x) 1)))
                             (select-list c (cdr pat) bl)))
                          (else (list)))))
		 ((ahash-ref match-term fpat)
		  (with upat (ahash-ref match-term fpat)
		    (select-list x (append upat (cdr pat)) bl)))
		 (else (with l (select-one x (car pat) bl)
			 (select-continue l (cdr pat)))))))
	(else (with l (select-one x (car pat) bl)
		(select-continue l (cdr pat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-select x pattern)
  "Select all subtrees of @x which match a given path pattern @pattern"
  (with sols (select-list x pattern '())
    ;; (display* "sols= " sols "\n")
    (map cadr sols)))

(varlet *texmacs-module* 'select tm-select)

(define-public (tm-ref t . l)
  (and (tm? t)
       (with r (select t l)
	 (and (nnull? r) (car r)))))
