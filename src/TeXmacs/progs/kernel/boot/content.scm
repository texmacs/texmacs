
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

(texmacs-module (kernel boot content)
  (:export
    atomic-tree? compound-tree? tree->list tree-explode
    tm? tm-atomic? tm-compound? tm-equal?
    tm-arity tm-car tm-cdr tm->list))

(define (atomic-tree? t)
  (and (tree? t) (tree-atomic? t)))

(define (compound-tree? t)
  (and (tree? t) (tree-compound? t)))

(define (tree->list t)
  (cons (tree-get-label t) (tree-get-children t)))

(define (tree-explode t)
  (if (atomic-tree? t)
      (tree->string t)
      (cons (tree-get-label t) (tree-get-children t))))

(define (tm? x)
  (or (string? x) (list? x) (tree? x)))

(define (tm-atomic? x)
  (or (string? x)
      (and (tree? x) (tree-atomic? x))))

(define (tm-compound? x)
  (or (pair? x)
      (and (tree? x) (tree-compound? x))))

(define (tm-equal? x y)
  (cond ((tree? x)
	 (if (tree? y)
	     (== x y)
	     (tm-equal? (tree-explode x) y)))
	((tree? y) (tm-equal? x (tree-explode y)))
	((and (pair? x) (pair? y))
	 (and (tm-equal? (car x) (car y))
	      (tm-equal? (cdr x) (cdr y))))
	(else (== x y))))

(define (tm-arity x)
  (cond ((list? x) (length x))
	((string? x) 0)
	(else (tree-arity x))))

(define (tm-car x)
  (car (if (pair? x) x (tree->list x))))

(define (tm-cdr x)
  (cdr (if (pair? x) x (tree->list x))))

(define (tm->list x)
  (if (list? x) x (tree->list x)))
