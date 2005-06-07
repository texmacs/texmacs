
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tree.scm
;; DESCRIPTION : routines for trees and for modifying documents
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library tree)
  (:use (utils library list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation inside trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-up t . opt)
  (:type (-> tree tree))
  (:synopsis "Get the parent of @t.")
  (let* ((p   (tree-path t))
	 (nr  (if (null? opt) 1 (car opt)))
	 (len (if (list? p) (length p) -1)))
    (and (>= len nr) (tm-subtree (list-head p (- len nr))))))

(tm-define (tree-down t . opt)
  (:type (-> tree tree))
  (:synopsis "Get the child where the cursor is.")
  (let* ((p   (tree-path t))
	 (q   (cDr (tm-where)))
	 (nr  (if (null? opt) 1 (car opt))))
    (and p (list-starts? (cDr q) p)
	 (>= (length q) (+ (length p) nr))
	 (tm-subtree (list-head q (+ (length p) nr))))))

(tm-define (tree-index t)
  (:type (-> tree int))
  (:synopsis "Get the child number of @t in its parent.")
  (with p (tree-path t)
    (and (pair? p) (cAr p))))

(tm-define (tree-down-index t)
  (:type (-> tree int))
  (:synopsis "Get the number the child where the cursor is.")
  (let ((p (tree-path t))
	(q (cDr (tm-where))))
    (and (list-starts? (cDr q) p)
	 (list-ref q (length p)))))

(tm-define (tree-inside? t ref)
  (:type (-> tree tree bool))
  (:synopsis "Is @t inside @ref?")
  (let ((p (tree-path ref))
	(q (tree-path t)))
    (and p q (list-starts? q p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-assign ref t)
  (:type (-> tree content void))
  (:synopsis "Assign @ref with @t.")
  ;;(display* "Assign " ref ", " t "\n")
  (with p (tree-path ref)
    (if p (tm-assign p t)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(tm-define-macro (tree-assign! ref t)
  (with var (gensym)
    `(with ,var ,t
       (tree-assign ,ref ,var)
       (set! ,ref ,var))))

(tm-define (tree-insert ref pos t)
  (:type (-> tree int content void))
  (:synopsis "Insert the children of @t into @ref at position @pos.")
  ;;(display* "Insert " ref ", " pos ", " t "\n")
  (with p (tree-path ref)
    (if p (tm-insert (rcons p pos) t)
	(texmacs-error "tree-insert" "~S is not part of a document" ref))))

(tm-define tree-insert! tree-insert)

(tm-define (tree-remove ref pos nr)
  (:type (-> tree int int void))
  (:synopsis "Remove @nr children from @ref at position @pos.")
  ;;(display* "Remove " ref ", " pos ", " nr "\n")
  (with p (tree-path ref)
    (if p (tm-remove (rcons p pos) nr)
	(texmacs-error "tree-remove" "~S is not part of a document" ref))))

(tm-define tree-remove! tree-remove)

(tm-define (tree-split ref pos at)
  (:type (-> tree int int void))
  (:synopsis "Split the @pos-th child of @ref at position @at.")
  ;;(display* "Split " ref ", " pos ", " at "\n")
  (with p (tree-path ref)
    (if p (tm-split (rcons* p pos at))
	(texmacs-error "tree-split" "~S is not part of a document" ref))))

(tm-define tree-split! tree-split)

(tm-define (tree-join ref pos)
  (:type (-> tree int void))
  (:synopsis "Split the @pos-th child of @ref with the next child.")
  ;;(display* "Join " ref ", " pos "\n")
  (with p (tree-path ref)
    (if p (tm-join (rcons p pos))
	(texmacs-error "tree-join" "~S is not part of a document" ref))))

(tm-define tree-join! tree-join)

(tm-define (tree-insert-node ref pos ins)
  (:type (-> tree int content void))
  (:synopsis "Transform @ref into @lab with @ref inserted at position @pos.")
  ;;(display* "Insert node " ref ", " pos ", " ins "\n")
  (with p (tree-path ref)
    (if p (tm-insert-node (rcons p pos) ins)
	(texmacs-error "tree-insert-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-insert-node! ref pos ins)
  (with var (gensym)
    `(with ,var (tree-path ,ref)
       (tree-insert-node ,ref ,pos ,ins)
       (set! ,ref (tm-subtree ,var)))))

(tm-define (tree-remove-node ref pos)
  (:type (-> tree int void))
  (:synopsis "Replace @ref by its @pos-th child.")
  ;;(display* "Remove node " ref ", " pos "\n")
  (with p (tree-path ref)
    (if p (tm-remove-node (rcons p pos))
	(texmacs-error "tree-remove-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-remove-node! ref pos)
  `(begin
     (tree-remove-node ,ref ,pos)
     (set! ,ref (tree-ref ,ref ,pos))))

(tm-define (tree-assign-node ref lab)
  (:type (-> tree symbol void))
  (:synopsis "Replace the label of @ref by @lab.")
  ;;(display* "Assign node " ref ", " lab "\n")
  (with p (tree-path ref)
    (if p (tm-assign-node p lab)
	(texmacs-error "tree-assign-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-assign-node! ref lab)
  (with var (gensym)
    `(with ,var (tree-path ,ref)
       (tree-assign-node ,ref ,lab)
       (set! ,ref (tm-subtree ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to use the above modification routines in an intelligent way
;; via a unique assignment routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-common-left t1 t2)
  (if (not (and (tm-compound? t1) (tm-compound? t2))) 0
      (list-common-left (cdr (tm->list t1)) (cdr (tm->list t2)))))

(define (tree-common-right t1 t2)
  (if (not (and (tm-compound? t1) (tm-compound? t2))) 0
      (list-common-right (cdr (tm->list t1)) (cdr (tm->list t2)))))

(define (tree-focus ref l)
  (cond ((null? l) #f)
	((tree-inside? (car l) ref) 0)
	((and (list? (car l)) (tree-focus ref (car l))) 0)
	(else
	 (with r (tree-focus ref (cdr l))
	   (if r (+ r 1) #f)))))

(define (tree-get-focus ref t l)
  (if (or (== t ref) (not (tree-inside? t ref)))
      (tree-focus ref l)
      (with r (tree-focus t l)
	(if r r (tree-get-focus ref (tree-up t) l)))))

(tm-define (tree-set-diff ref t)
  (:type (-> tree content void))
  (:synopsis "Assign @ref with @t.")
  (let ((p (tree-path ref))
	(l (tree-common-left ref t))
	(r (tree-common-right ref t)))
    (cond ((not p)
	   (texmacs-error "tree-set-diff" "~S isn't part of a document" ref))
	  ((tm-equal? ref t) (noop))
	  ((tree-inside? t ref)
	   (with q (tree-path t)
	     (tree-remove-node! ref (list-ref q (length p)))
	     (tree-set-diff ref t)))
	  ((nlist? t) (tree-assign ref t))
	  ((and (= l (tm-arity ref)) (= l (tm-arity t)))
	   (tree-assign-node ref (tm-car t)))
	  ((and (= (+ l r) (tm-arity ref)) (< (tm-arity ref) (tm-arity t)))
	   (tree-insert! ref l (cons (tm-car ref)
				     (sublist (cdr t) l (- (tm-arity t) r))))
	   (if (!= (tm-car ref) (car t)) (tree-assign-node ref (tm-car t))))
	  ((and (= (+ l r) (tm-arity t)) (> (tm-arity ref) (tm-arity t)))
	   (tree-remove! ref l (- (- (tm-arity ref) r) l))
	   (if (!= (tm-car ref) (car t)) (tree-assign-node ref (tm-car t))))
	  (else
	   (with pos (tree-focus ref (cdr t))
	     (if (not pos) (tree-assign ref t)
		 (let ((head (list-head t (+ pos 1)))
		       (mid  (list-ref t (+ pos 1)))
		       (tail (list-tail t (+ pos 2))))
		   (tree-set-diff! ref mid)
		   (tree-insert-node ref pos (append head tail)))))))))

(tm-define-macro (tree-set-diff! ref t)
  (with var (gensym)
    `(with ,var (tree-path ,ref)
       (tree-set-diff ,ref ,t)
       (set! ,ref (tm-subtree ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-ref t . l)
  (:synopsis "Access a subtree of @t according to @l.")
  (if (and (list-1? l) (integer? (car l)))
      (tree-get-child t (car l))
      (with r (tm-select t l)
	(if (null? r) #f
	    (car r)))))

(tm-define (tree-set t . args)
  (:synopsis "Set a subtree of @t to a new value according to @l.")
  (let ((l (cDr args))
	(u (cAr args)))
    (with r (tm-select t l)
      (if (nnull? r)
	  (tree-set-diff (car r) u)))))

(tm-define-macro (tree-set! t . l)
  (if (list-1? l)
      `(tree-set-diff! ,t ,@l)
      `(tree-set ,t ,@l)))

(define (tree-innermost-sub p labs)
  (with t (tm-subtree p)
    (cond ((and (tm-compound? t) (in? (tree-get-label t) labs)) t)
	  ((or (null? p) (== p (buffer-path))) #f)
	  (else (tree-innermost-sub (cDr p) labs)))))

(tm-define (tree-innermost l)
  (:type (-> symbol tree))
  (:synopsis "Search upwards from a tree or the cursor position.")
  (let* ((p (cDDr (tm-where)))
	 (labs (if (list? l) l (list l))))
    (tree-innermost-sub p labs)))

(tm-define (inside-which l)
  (:type (-> (list symbol) symbol))
  (:synopsis "Get innermost node type among possibilities in @l.")
  (with t (tree-innermost l)
    (and t (tree-get-label t))))

(tm-define-macro (with-innermost t lab . body)
  `(let ((,t (tree-innermost ,lab)))
     (if ,t (begin ,@body))))

(tm-define (tree-go-to t . l)
  (:synopsis "Go to a position determined by @l inside the tree @t.")
  (let* ((u (apply tree-ref (cons t (cDr l))))
	 (i (cAr l)))
    (cond ((not u) (noop))
	  ((== i :start) (tm-go-to (tm-start (tree-path u))))
	  ((== i :end) (tm-go-to (tm-end (tree-path u))))
	  ((integer? i) (tm-go-to (rcons (tree-path u) i)))
	  (else (noop)))))

(tm-define (tree-correct t)
  (with p (tree-path t)
    (if p (tm-correct p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cursor-tree)
  (tm-subtree (cDr (cursor-path))))

(define the-action-path '(-1))

(tm-define (action-set-path p)
  (set! the-action-path p))

(tm-define (action-path)
  (and (!= the-action-path '(-1)) the-action-path))

(tm-define (action-tree)
  (and (!= the-action-path '(-1)) (tm-subtree the-action-path)))

(tm-define-macro (with-action t . body)
  `(with ,t (action-tree)
     (if ,t (begin ,@body))))

(tm-define (mutator-tree)
  (with p (mutator-path)
    (if (nnull? p) (tm-subtree p) #f)))

(tm-define-macro (with-mutator t . body)
  `(with ,t (mutator-tree)
     (if ,t (begin ,@body))))
