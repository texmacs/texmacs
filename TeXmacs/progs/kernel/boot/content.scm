
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

(texmacs-module (kernel boot content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines on trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree . l)
  (if (string? (car l))
      (string->tree (car l))
      (tm->tree l)))

(define-public (atomic-tree? t)
  (and (tree? t) (tree-atomic? t)))

(define-public (compound-tree? t)
  (and (tree? t) (tree-compound? t)))

(define-public (tree->list t)
  (cons (tree-label t) (tree-children t)))

(define-public (tree-explode t)
  (if (atomic-tree? t)
      (tree->string t)
      (cons (tree-label t) (tree-children t))))

(define-public (tree-get-path t)
  (and (tree? t)
       (let ((ip (tree-ip t)))
	 (and (or (null? ip) (!= (cAr ip) -5))
	      (reverse ip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines for positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (position-new . opts)
  (position-new-path (if (null? opts) (cursor-path) (car opts))))

(define-public-macro (with-cursor p . body)
  (let* ((pos (gensym))
	 (res (gensym)))
    `(with ,pos (position-new)
       (position-set ,pos (cursor-path))
       (go-to ,p)
       (with ,res (begin ,@body)
	 (go-to (position-get ,pos))
	 (position-delete ,pos)
	 ,res))))

(define-public-macro (cursor-after . body)
  (let* ((pos (gensym))
	 (res (gensym)))
    `(with ,pos (position-new)
       (position-set ,pos (cursor-path))
       ,@body
       (with ,res (cursor-path)
	 (go-to (position-get ,pos))
	 (position-delete ,pos)
	 ,res))))

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

(define-public (tm-arity x)
  (cond ((list? x) (- (length x) 1))
	((string? x) 0)
	(else (tree-arity x))))

(define-public (tm-car x)
  (car (if (pair? x) x (tree->list x))))

(define-public (tm-cdr x)
  (cdr (if (pair? x) x (tree->list x))))

(define-public (tm->list x)
  (if (list? x) x (tree->list x)))

(define-public (tm-func? x . args)
  (or (and (list? x) (apply func? (cons x args)))
      (and (compound-tree? x) (apply func? (cons (tree->list x) args)))))

(define-public (tm-is? x lab)
  (or (and (pair? x) (== (car x) lab))
      (and (compound-tree? x) (== (tree-label x) lab))))

(define-public (tm-in? x l)
  (or (and (pair? x) (in? (car x) l))
      (and (compound-tree? x) (in? (tree-label x) l))))
