
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
  (:use (kernel texmacs tm-define) (kernel library list))
  (:export
    tree-path tree-assign tree-assign!
    tree-insert tree-insert! tree-remove tree-remove!
    tree-split tree-split! tree-join tree-join!
    tree-ins-unary tree-ins-unary! tree-rem-unary tree-rem-unary!))

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

(define-macro (tree-assign! ref t)
  `(with xxx-t ,t
     (tree-assign ,ref xxx-t)
     (set! ,ref xxx-t)))

(tm-define (tree-insert ref pos t)
  (:type (-> tree int tree void))
  (:synopsis "Insert the children of @t into @ref at position @pos.")
  (with p (tree-path ref)
    (if p (tm-insert (rcons p pos) t)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define tree-insert! tree-insert)

(tm-define (tree-remove ref pos nr)
  (:type (-> tree int int void))
  (:synopsis "Remove @nr children from @ref at position @pos.")
  (with p (tree-path ref)
    (if p (tm-remove (rcons p pos) nr)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define tree-remove! tree-remove)

(tm-define (tree-split ref pos at)
  (:type (-> tree int int void))
  (:synopsis "Split the @pos-th child of @ref at position @at.")
  (with p (tree-path ref)
    (if p (tm-split (rcons* p pos at))
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define tree-split! tree-split)

(tm-define (tree-join ref pos)
  (:type (-> tree int void))
  (:synopsis "Split the @pos-th child of @ref with the next child.")
  (with p (tree-path ref)
    (if p (tm-join (rcons p pos))
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define tree-join! tree-join)

(tm-define (tree-ins-unary ref lab)
  (:type (-> tree symbol void))
  (:synopsis "Transform @ref into the tree @(lab ref).")
  (with p (tree-path ref)
    (if p (tm-ins-unary p lab)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define-macro (tree-ins-unary! ref lab)
  `(with xxx-p (tree-path ,ref)
     (tree-ins-unary ,ref ,lab)
     (set! ,ref (tm-subtree xxx-p))))

(tm-define (tree-rem-unary ref)
  (:type (-> tree symbol void))
  (:synopsis "Set a unary tree @ref to its unique child.")
  (with p (tree-path ref)
    (if p (tm-rem-unary p)
	(texmacs-error "tree-assign" "~S is not part of a document" ref))))

(define-macro (tree-rem-unary! ref)
  `(begin
     (tree-rem-unary ,ref)
     (set! ,ref (tree-ref ,ref 0))))
