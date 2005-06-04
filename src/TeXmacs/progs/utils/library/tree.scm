
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
;; Paths associated to trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-path t)
  (:type (-> tree path))
  (:synopsis "Get the path associated to @t or #f.")
  (with ip (tree-ip t)
    (if (== (cAr ip) -5) #f
	(reverse ip))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-assign ref t)
  (:type (-> tree tree void))
  (:synopsis "Assign @ref with @t.")
  (with p (tree-path ref)
    (if p (tm-assign p t)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(tm-define-macro (tree-assign! ref t)
  (with var (gensym)
    `(with ,var ,t
       (tree-assign ,ref ,var)
       (set! ,ref ,var))))

(tm-define (tree-insert ref pos t)
  (:type (-> tree int tree void))
  (:synopsis "Insert the children of @t into @ref at position @pos.")
  (with p (tree-path ref)
    (if p (tm-insert (rcons p pos) t)
	(texmacs-error "tree-insert" "~S is not part of a document" ref))))

(tm-define tree-insert! tree-insert)

(tm-define (tree-remove ref pos nr)
  (:type (-> tree int int void))
  (:synopsis "Remove @nr children from @ref at position @pos.")
  (with p (tree-path ref)
    (if p (tm-remove (rcons p pos) nr)
	(texmacs-error "tree-remove" "~S is not part of a document" ref))))

(tm-define tree-remove! tree-remove)

(tm-define (tree-split ref pos at)
  (:type (-> tree int int void))
  (:synopsis "Split the @pos-th child of @ref at position @at.")
  (with p (tree-path ref)
    (if p (tm-split (rcons* p pos at))
	(texmacs-error "tree-split" "~S is not part of a document" ref))))

(tm-define tree-split! tree-split)

(tm-define (tree-join ref pos)
  (:type (-> tree int void))
  (:synopsis "Split the @pos-th child of @ref with the next child.")
  (with p (tree-path ref)
    (if p (tm-join (rcons p pos))
	(texmacs-error "tree-join" "~S is not part of a document" ref))))

(tm-define tree-join! tree-join)

(tm-define (tree-insert-node ref pos ins)
  (:type (-> tree int tree void))
  (:synopsis "Transform @ref into @lab with @ref inserted at position @pos.")
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
  (with p (tree-path ref)
    (if p (tm-assign-node p lab)
	(texmacs-error "tree-assign-node" "~S isn't part of a document" ref))))

(tm-define-macro (tree-assign-node! ref lab)
  (with var (gensym)
    `(with ,var (tree-path ,ref)
       (tree-assign-node ,ref ,lab)
       (set! ,ref (tm-subtree ,var)))))
