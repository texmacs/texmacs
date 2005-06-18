
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
;; Navigation inside trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-up t . opt)
  "Get the parent of @t."
  (let* ((p   (tree->path t))
	 (nr  (if (null? opt) 1 (car opt)))
	 (len (if (list? p) (length p) -1)))
    (and (>= len nr) (path->tree (list-head p (- len nr))))))

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
  "Get the number the child where the cursor is."
  (let ((p (tree->path t))
	(q (cDr (cursor-path))))
    (and (list-starts? (cDr q) p)
	 (list-ref q (length p)))))

(define-public (tree-inside? t ref)
  "Is @t inside @ref?"
  (let ((p (tree->path ref))
	(q (tree->path t)))
    (and p q (list-starts? q p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-assign ref t)
  "Assign @ref with @t."
  ;;(display* "Assign " ref ", " t "\n")
  (with p (tree->path ref)
    (if p (path-assign p t)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define-public-macro (tree-assign! ref t)
  (with var (gensym)
    `(with ,var ,t
       (tree-assign ,ref ,var)
       (set! ,ref ,var))))

(define-public (tree-insert ref pos t)
  "Insert the children of @t into @ref at position @pos."
  ;;(display* "Insert " ref ", " pos ", " t "\n")
  (with p (tree->path ref)
    (if p (path-insert (rcons p pos) t)
	(texmacs-error "tree-insert" "~S is not part of a document" ref))))

(define-public tree-insert! tree-insert)

(define-public (tree-remove ref pos nr)
  "Remove @nr children from @ref at position @pos."
  ;;(display* "Remove " ref ", " pos ", " nr "\n")
  (with p (tree->path ref)
    (if p (path-remove (rcons p pos) nr)
	(texmacs-error "tree-remove" "~S is not part of a document" ref))))

(define-public tree-remove! tree-remove)

(define-public (tree-split ref pos at)
  "Split the @pos-th child of @ref at position @at."
  ;;(display* "Split " ref ", " pos ", " at "\n")
  (with p (tree->path ref)
    (if p (path-split (rcons* p pos at))
	(texmacs-error "tree-split" "~S is not part of a document" ref))))

(define-public tree-split! tree-split)

(define-public (tree-join ref pos)
  "Split the @pos-th child of @ref with the next child."
  ;;(display* "Join " ref ", " pos "\n")
  (with p (tree->path ref)
    (if p (path-join (rcons p pos))
	(texmacs-error "tree-join" "~S is not part of a document" ref))))

(define-public tree-join! tree-join)

(define-public (tree-insert-node ref pos ins)
  "Transform @ref into @lab with @ref inserted at position @pos."
  ;;(display* "Insert node " ref ", " pos ", " ins "\n")
  (with p (tree->path ref)
    (if p (path-insert-node (rcons p pos) ins)
	(texmacs-error "tree-insert-node" "~S isn't part of a document" ref))))

(define-public-macro (tree-insert-node! ref pos ins)
  (with var (gensym)
    `(with ,var (tree->path ,ref)
       (tree-insert-node ,ref ,pos ,ins)
       (set! ,ref (path->tree ,var)))))

(define-public (tree-remove-node ref pos)
  "Replace @ref by its @pos-th child."
  ;;(display* "Remove node " ref ", " pos "\n")
  (with p (tree->path ref)
    (if p (path-remove-node (rcons p pos))
	(texmacs-error "tree-remove-node" "~S isn't part of a document" ref))))

(define-public-macro (tree-remove-node! ref pos)
  `(begin
     (tree-remove-node ,ref ,pos)
     (set! ,ref (tree-ref ,ref ,pos))))

(define-public (tree-assign-node ref lab)
  "Replace the label of @ref by @lab."
  ;;(display* "Assign node " ref ", " lab "\n")
  (with p (tree->path ref)
    (if p (path-assign-node p lab)
	(texmacs-error "tree-assign-node" "~S isn't part of a document" ref))))

(define-public-macro (tree-assign-node! ref lab)
  (with var (gensym)
    `(with ,var (tree->path ,ref)
       (tree-assign-node ,ref ,lab)
       (set! ,ref (path->tree ,var)))))
