
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : content.scm
;; DESCRIPTION : important subroutines for manipulating content
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-atomic? x)
  (or (string? x)
      (and (tree? x) (tree-atomic? x))))

(define-public (tm-compound? x)
  (or (pair? x)
      (and (tree? x) (tree-compound? x))))

(define-public (tm-equal? x y)
  (cond ((tree? x)
	 (if (tree? y)
	     (== x y)
	     (tm-equal? (tree-explode x) y)))
	((tree? y) (tm-equal? x (tree-explode y)))
	((and (pair? x) (pair? y))
	 (and (tm-equal? (car x) (car y))
	      (tm-equal? (cdr x) (cdr y))))
	(else (== x y))))

(define-public (tm-length x)
  (cond ((string? x) (string-length x))
	((list? x) (- (length x) 1))
	((tree-atomic? x) (string-length (tree->string x)))
	(else (tree-arity x))))

(define-public (tm-arity x)
  (cond ((list? x) (- (length x) 1))
	((string? x) 0)
	(else (tree-arity x))))

(define-public (tm->string x)
  (if (string? x) x (tree->string x)))

(define-public (tm->list x)
  (if (list? x) x (tree->list x)))

(define-public (tm->stree t)
  (tree->stree (tm->tree t)))

(define-public (tm-car x)
  (if (pair? x) (car x) (tree-label x)))

(define-public (tm-cdr x)
  (cdr (if (pair? x) x (tree->list x))))

(define-public (tm-range x from to)
  (cond ((string? x) (substring x from to))
	((list? x) (cons (car x) (sublist (cdr x) from to)))
	((tree-atomic? x) (substring (tm->string x) from to))
	(else (cons (tm-car x) (sublist (tm-cdr x) from to)))))

(define-public (tm-func? x . args)
  (or (and (list? x) (apply func? (cons x args)))
      (and (compound-tree? x) (apply func? (cons (tree->list x) args)))))

(define-public (tm-is? x lab)
  (or (and (pair? x) (== (car x) lab))
      (and (compound-tree? x) (== (tree-label x) lab))))

(define-public (tm-in? x l)
  (or (and (pair? x) (in? (car x) l))
      (and (compound-tree? x) (in? (tree-label x) l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs lengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-length-unit-search s pos)
  (cond ((nstring? s) #f)
	((>= pos (string-length s)) #f)
	((char-alphabetic? (string-ref s pos)) pos)
	(else (tm-length-unit-search s (+ pos 1)))))

(define-public (tm-make-length val unit)
  (string-append (number->string val) unit))

(define-public (tm-length? s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length? (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(and (string-number? (substring s 0 pos))
	     (string-locase-alpha? (substring s pos (string-length s)))))))

(define-public (tm-length-value s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length-value (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(string->number (substring s 0 pos)))))

(define-public (tm-length-unit s)
  (if (tree? s)
      (and (tree-atomic? s) (tm-length-unit (tree->string s)))
      (and-with pos (tm-length-unit-search s 0)
	(substring s pos (string-length s)))))
