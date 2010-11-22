
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text std-text-edit)
  (:use (utils library tree)
	(utils edit variants)
	(text std-text-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a title and an abstract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-propose-title?)
  (with bt (buffer-tree)
    (and-with t (tree-ref bt :down)
      (and (tree-is? bt 'document)
	   (== (tree-index t) 0)
	   (not (tree-in? t '(doc-data tmdoc-title)))))))

(tm-define (document-propose-abstract?)
  (with bt (buffer-tree)
    (and-with t (tree-ref bt :down)
      (and (tree-is? bt 'document)
	   (<= (tree-index t) 1)
	   (tree-is? bt 0 'doc-data)
	   (or (== (tree-arity bt) 1)
	       (not (tree-is? bt 1 'abstract)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting document and author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-title-context? t)
  (or (tree-in? t (doc-title-tag-list))
      (and (tree-is? t 'date) (tree-is? t :up 'doc-date))))

(tm-define (doc-author-context? t)
  (tree-in? t (doc-author-tag-list)))

(define doc-data-inactive-tags
  (doc-title-inactive-tag-list))

(tm-define (make-doc-data)
  (insert-go-to '(doc-data (doc-title "")) '(0 0 0)))

(tm-define (make-doc-data-element l)
  (with-innermost t 'doc-data
    (with pos (1+ (tree-down-index t))
      (cond ((== l 'doc-author-data)
	     (tree-insert! t pos `((,l (author-name ""))))
	     (tree-go-to t pos 0 0 0))
	    ((== l 'doc-note)
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    ((in? l doc-data-inactive-tags)
	     (tree-insert! t pos `((doc-inactive (,l ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (make-author-data-element l)
  (with-innermost t 'doc-author-data
    (with pos (1+ (tree-down-index t))
      (cond ((in? l '(author-address author-note))
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'title))
  (go-end-line)
  (insert-return))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-title))
  (make-doc-data-element 'doc-author-data))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'author-name))
  (make-author-data-element 'author-address))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-inactive))
  (doc-data-activate-here))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activation and disactivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doc-data-go-to-active t i)
  (cond ((< i 0) (tree-go-to t :end))
	((tree-in? t i (doc-title-inactive-tag-list))
	 (doc-data-go-to-active t (- i 1)))
	((not (cursor-inside? (tree-ref t i)))
	 (tree-go-to t i :end))))

(tm-define (doc-data-activate-here)
  (with-innermost dd 'doc-data
    (with-innermost t 'doc-inactive
      (tree-remove-node! t 0)
      (doc-data-go-to-active dd (tree-down-index dd)))))

(tm-define (doc-data-has-hidden?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (with fun (lambda (t) (or (tree-in? t (doc-title-inactive-tag-list))
				(tree-is? t 'doc-inactive)))
	(list-or (map fun l))))))

(tm-define (doc-data-disactivated?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (list-or (map (lambda (t) (== (tm-car t) 'doc-inactive)) l)))))

(define (doc-data-activate-one t)
  (when (tree-is? t 'doc-inactive)
    (tree-remove-node! t 0)))

(tm-define (doc-data-activate-all)
  (with-innermost t 'doc-data
    (with i (tree-down-index t)
      (with l (cdr (tree->list t))
	(for-each doc-data-activate-one l))
      (doc-data-go-to-active t i))))

(define (doc-data-disactivate-one t)
  (if (in? (tm-car t) doc-data-inactive-tags)
      (tree-insert-node! t 0 '(doc-inactive))))

(tm-define (doc-data-disactivate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-disactivate-one l))))

(tm-define (doc-data-activate-toggle)
  (if (doc-data-disactivated?)
      (doc-data-activate-all)
      (doc-data-disactivate-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making letter headings or titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-end-of-header-element)
  (if (inside? 'address) (go-end-of 'address))
  (if (inside? 'destination) (go-end-of 'destination))
  (if (inside? 'cc) (go-end-of 'cc))
  (if (inside? 'encl) (go-end-of 'encl))
  (go-end-line))

(tm-define (make-header l)
  (go-end-of-header-element)
  (if (!= (tree->stree (paragraph-tree)) "") (insert-return))
  (make l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sectional commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (section-context? t)
  (tree-in? t (numbered-unnumbered-append (section-tag-list))))

(tm-define (previous-section)
  (with bt (buffer-tree)
    (and (cursor-inside? bt)
	 (with bp (list-drop (cursor-path) (length (tree->path bt)))
	   (with sp (path-previous-section bt bp)
	     (and (!= sp bp) (path->tree (append (tree->path bt) sp))))))))

(tm-define (make-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)))

(tm-define (make-unnamed-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)
      (make-return-before)))

(tm-define (kbd-enter t shift?)
  (:require (section-context? t))
  (tree-go-to t :end)
  (insert-return))

(tm-define (label-insert t)
  (:require (section-context? t))
  (tree-go-to t :end)
  (make 'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists, enumerations and description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-context? t)
  (tree-in? t (list-tag-list)))

(tm-define (itemize-context? t)
  (tree-in? t (itemize-tag-list)))

(tm-define (enumerate-context? t)
  (tree-in? t (enumerate-tag-list)))

(tm-define (itemize-enumerate-context? t)
  (or (tree-in? t (itemize-tag-list))
      (tree-in? t (enumerate-tag-list))))

(tm-define (make-tmlist l)
  (make l)
  (make-item))

(tm-define (make-item)
  (if (not (make-return-after))
      (with lab (inside-which (list-tag-list))
	(cond ((in? lab (itemize-tag-list)) (make 'item))
	      ((in? lab (enumerate-tag-list)) (make 'item))
	      ((in? lab (description-tag-list)) (make 'item*))))))

(tm-define (kbd-enter t shift?)
  (:require (list-context? t))
  (make-item))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'item*))
  (go-end-of 'item*))

(tm-define (numbered-context? t)
  (:require (or (itemize-context? t) (enumerate-context? t)))
  #t)

(tm-define (numbered-numbered? t)
  (:require (enumerate-context? t))
  #t)

(tm-define (numbered-toggle t)
  (:require (itemize-context? t))
  (variant-set t 'enumerate))

(tm-define (numbered-toggle t)
  (:require (enumerate-context? t))
  (variant-set t 'itemize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-equation)
  (make 'equation)
  (temp-proof-fix))

(tm-define (make-equation*)
  (make 'equation*)
  (temp-proof-fix))

(tm-define (make-eqnarray*)
  (make 'eqnarray*)
  (temp-proof-fix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-aux env aux)
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux '(document "")))))

(tm-define (make-aux* env aux name)
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux name '(document "")))))

(tm-define (make-bib style file-name)
  (:argument style "Bibliography style")
  (:proposals style '("tm-plain" "tm-alpha" "tm-ieeetr" "tm-siam"))
  (:argument file-name "Bibliography file")
  (if (not (make-return-after))
      (insert (list 'bibliography "bib" style file-name '(document "")))))
