
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

(define-public (tree-is? t lab)
  (== (tree-label t) lab))

(define-public (tree->list t)
  (cons (tree-label t) (tree-children t)))

(define-public (tree-explode t)
  (if (atomic-tree? t)
      (tree->string t)
      (cons (tree-label t) (tree-children t))))

(define-public (tree->path t)
  (and (tree? t)
       (let ((ip (tree-ip t)))
	 (if (== (cAr ip) -5) #f
	     (reverse ip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines for positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (position-new . opts)
  (position-new-path (if (null? opts) (cursor-path) (car opts))))

(define-public-macro (with-cursor p . body)
  (with pos (gensym)
    `(with ,pos (position-new)
       (position-set ,pos (cursor-path))
       (go-to ,p)
       ,@body
       (go-to (position-get ,pos))
       (position-delete ,pos))))

(define (apply-go-to routine)
  (with p (routine (root-tree) (cursor-path))
    (if (list-starts? (cDr p) (buffer-path)) (go-to p))))

(define-public (go-to-next) (apply-go-to path-next))
(define-public (go-to-previous) (apply-go-to path-previous))
(define-public (go-to-next-word) (apply-go-to path-next-word))
(define-public (go-to-previous-word) (apply-go-to path-previous-word))
(define-public (go-to-next-node) (apply-go-to path-next-node))
(define-public (go-to-previous-node) (apply-go-to path-previous-node))
(define-public (go-to-next-tag lab)
  (apply-go-to (lambda (t p) (path-next-tag t p lab))))
(define-public (go-to-previous-tag lab)
  (apply-go-to (lambda (t p) (path-previous-tag t p lab))))

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
  (cond ((list? x) (apply func? (cons x args)))
	((compound-tree? x) (apply func? (cons (tree->list x) args)))
	(else #f)))
