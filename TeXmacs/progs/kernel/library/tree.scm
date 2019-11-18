
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tree.scm
;; DESCRIPTION : routines for trees and for modifying documents
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library tree)
  (:use (kernel library list)))

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

(define-public (tree->symbol t)
  (string->symbol (tree->string t)))

(define-public (tree-number? t)
  (and (tree-atomic? t) (string->number (tree->string t))))

(define-public (tree-integer? t)
  (and (tree-atomic? t) (integer? (string->number (tree->string t)))))

(define-public (tree->number t)
  (if (tree-atomic? t) (string->number (tree->string t)) 0))

(define-public (tree-explode t)
  (if (atomic-tree? t)
      (tree->string t)
      (cons (tree-label t) (tree-children t))))

(define-public (tree-get-path t)
  (and (tree? t)
       (let ((ip (tree-ip t)))
	 (and (or (null? ip) (!= (cAr ip) -5))
	      (reverse ip)))))

(define-public (tree-func? t . args)
  (and (compound-tree? t)
       (apply func? (cons (tree->list t) args))))

(define-public (tree-map-children fun t)
  (tm->tree `(,(tree-label t)
              ,@(map fun (tree-children t)))))

(define-public (tree-map-accessible-children fun t)
  (with rew (lambda (i)
              (if (tree-accessible-child? t i)
                  (fun (tree-ref t i))
                  (tree-ref t i)))
    (tm->tree `(,(tree-label t)
                ,@(map rew (.. 0 (tree-arity t)))))))

(define-public (tree-search t pred?)
  (with me (if (pred? t) (list t) '())
    (if (tree-atomic? t) me
	(append me (append-map (cut tree-search <> pred?)
			       (tree-children t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation inside trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-up t . opt)
  "Get the parent of @t."
  (let* ((p   (tree->path t))
	 (nr  (if (null? opt) 1 (car opt)))
	 (len (if (list? p) (length p) -1)))
    (and (>= len nr) (path->tree (list-head p (- len nr))))))

(define-public (tree-outer t)
  "Get parent of @t except for buffer trees"
  (and (not (tree-is-buffer? t))
       (tree-up t)))

(define-public (tree-down t . opt)
  "Get the child where the cursor is."
  (let* ((p   (tree->path t))
	 (q   (cDr (cursor-path)))
	 (nr  (if (null? opt) 1 (car opt))))
    (and p (list-starts? (cDr q) p)
	 (>= (length q) (+ (length p) nr))
	 (path->tree (list-head q (+ (length p) nr))))))

(define-public (tree-index t)
  "Get the child number of @t in its parent."
  (with p (tree->path t)
    (and (pair? p) (cAr p))))

(define-public (tree-down-index t)
  "Get the number of the child where the cursor is."
  (let ((p (tree->path t))
	(q (cDr (cursor-path))))
    (and (list-starts? (cDr q) p)
	 (list-ref q (length p)))))

(define-public (tree-inside? t ref)
  "Is @t inside @ref?"
  (let ((p (tree->path ref))
	(q (tree->path t)))
    (and p q (list-starts? q p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor related trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (cursor-tree)
  (path->tree (cDr (cursor-path))))

(define-public (cursor-tree*)
  (path->tree (cDr (cursor-path*))))

(define-public (before-cursor)
  (let* ((t (cursor-tree))
	 (i (cAr (cursor-path))))
    (cond ((and (tree-atomic? t) (> i 0))
	   (with s (tree->string t)
	     (with j (string-previous s i)
	       (substring s j i))))
	  ((tree-atomic? t) #f)
	  ((> i 0) t)
	  (else #f))))

(define-public (after-cursor)
  (let* ((t (cursor-tree*))
	 (i (cAr (cursor-path*))))
    (cond ((and (tree-atomic? t) (< i (string-length (tree->string t))))
	   (with s (tree->string t)
	     (with j (string-next s i)
	       (substring s i j))))
	  ((tree-atomic? t) #f)
	  ((== i 0) t)
	  (else #f))))

(define-public (focus-tree)
  (path->tree (get-focus-path)))

(define-public (cursor-on-border? t)
  (let* ((p (cursor-path))
         (i (cAr p)))
    (and (== (cDr p) (tree->path t))
         (or (== i 0)
             (if (tree-atomic? t)
                 (== i (string-length (tree->string t)))
                 (== i 1))))))

(define-public (cursor-inside? t)
  (let* ((c (cursor-path))
         (p (cDr c))
         (q (tree->path t)))
    (and (list? q)
         (if (tree-atomic? (cursor-tree))
             (>= (length p) (length q))
             (> (length p) (length q)))
         (== (sublist p 0 (length q)) q)
         (sublist c (length q) (length c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other special trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (table-cell-tree row col)
  (path->tree (table-cell-path row col)))

(define the-action-tree #f)

(define-public (exec-delayed-at cmd t)
  (with old-t the-action-tree
    (set! the-action-tree t)
    (exec-delayed (lambda () (cmd) (set! the-action-tree old-t)))))

(define-public (action-tree)
  the-action-tree)

(define-public-macro (with-action t . body)
  `(and-with ,t (action-tree)
     ,@body))
