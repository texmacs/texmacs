
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
;; Inserting document and author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-data-inactive-tags
  '(doc-running-title doc-running-author doc-keywords doc-AMS-class))

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

(tm-define (kbd-return)
  (:inside title)
  (go-end-line)
  (insert-return))

(tm-define (kbd-return)
  (:inside doc-title)
  (make-doc-data-element 'doc-author-data))

(tm-define (kbd-return)
  (:inside author-name)
  (make-author-data-element 'author-address))

(tm-define (kbd-return)
  (:inside doc-inactive)
  (doc-data-activate-here))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activation and disactivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-data-activate-here)
  (with-innermost t 'doc-inactive
    (tree-remove-node! t 0)
    (with-innermost t 'doc-data
      (tree-go-to t :start))))

(tm-define (doc-data-disactivated?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (list-or (map (lambda (t) (== (tm-car t) 'doc-inactive)) l)))))

(define (doc-data-activate-one t)
  (if (== (tm-car t) 'doc-inactive)
      (tree-remove-node! t 0)))

(tm-define (doc-data-activate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-activate-one l))))

(define (doc-data-disactivate-one t)
  (if (in? (tm-car t) doc-data-inactive-tags)
      (tree-insert-node! t 0 '(doc-inactive))))

(tm-define (doc-data-disactivate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-disactivate-one l))))

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

(define (section-context? t)
  (tree-in? t (numbered-unnumbered-append (section-tag-list))))

(tm-define (make-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)))

(tm-define (make-unnamed-section l)
  (if (or (selection-active-any?) (not (make-return-after)))
      (make l)
      (make-return-before)))

(tm-define (kbd-return)
  (:context section-context?)
  (with-innermost t section-context?
    (tree-go-to t :end)
    (insert-return)))

(tm-define (make-label)
  (:context section-context?)
  (with-innermost t section-context?
    (tree-go-to t :end)
    (make 'label)))

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

(tm-define (kbd-return)
  (:context list-context?)
  (make-item))

(tm-define (kbd-return)
  (:inside item*)
  (go-end-of 'item*))

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
