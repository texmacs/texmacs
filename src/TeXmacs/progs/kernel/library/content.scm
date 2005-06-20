
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : content.scm
;; DESCRIPTION : important subroutines for manipulating content
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for general content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm? x)
  (or (string? x) (list? x) (tree? x)))

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

(define-public (tm-car x)
  (car (if (pair? x) x (tree->list x))))

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
