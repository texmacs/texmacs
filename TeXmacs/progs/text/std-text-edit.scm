
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text std-text-edit)
  (:use (utils edit variants)))

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
	     (tree-insert t pos `(doc-data (,l (author-name ""))))
	     (tree-go-to t pos 0 0 0))
	    ((== l 'doc-note)
	     (tree-insert t pos `(doc-data (,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    ((in? l doc-data-inactive-tags)
	     (tree-insert t pos `(doc-data (doc-inactive (,l ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert t pos `(doc-data (,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (make-author-data-element l)
  (with-innermost t 'doc-author-data
    (with pos (1+ (tree-down-index t))
      (cond ((in? l '(author-address author-note))
	     (tree-insert t pos `(doc-author-data (,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert t pos `(doc-author-data (,l "")))
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
    (tree-remove-node t 0)))

(tm-define (doc-data-disactivated?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (list-or (map (lambda (t) (== (tm-car t) 'doc-inactive)) l)))))

(define (doc-data-activate-one t)
  (if (== (tm-car t) 'doc-inactive)
      (tree-remove-node t 0)))

(tm-define (doc-data-activate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-activate-one l))))

(define (doc-data-disactivate-one t)
  (if (in? (tm-car t) doc-data-inactive-tags)
      (tree-insert-node t 0 '(doc-inactive))))

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

(tm-define (inside-section?)
  (or (inside? 'part)
      (inside? 'part*)
      (inside? 'chapter)
      (inside? 'chapter*)
      (inside? 'appendix)
      (inside? 'section)
      (inside? 'section*)
      (inside? 'subsection)
      (inside? 'subsection*)
      (inside? 'subsubsection)
      (inside? 'subsubsection*)
      (inside? 'paragraph)
      (inside? 'paragraph*)
      (inside? 'subparagraph)
      (inside? 'subparagraph*)))

(tm-define (make-section l)
  (if (not (make-return-after))
      (make l)))

(tm-define (make-unnamed-section l)
  (if (not (make-return-after))
      (make l)
      (make-return-before)))

(tm-define (kbd-return)
  (:inside part part* chapter chapter* appendix
	   section subsection subsubsection
	   section* subsection* subsubsection*
	   paragraph subparagraph paragraph* subparagraph*)
  (go-end-line)
  (insert-return))

(define (toggle-number-sub s)
  (with s* (string->symbol (string-append (symbol->string s) "*"))
    (cond ((inside? s) (variant-replace s s*))
	  ((inside? s*) (variant-replace s* s)))))

(tm-define (toggle-section-number)
  (for-each
   toggle-number-sub
   '(part chapter section subsection subsubsection
     paragraph subparagraph appendix

     theorem proposition lemma corollary
     axiom definition notation conjecture
     remark example note warning convention
     exercise problem
     small-figure big-figure small-table big-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists, enumerations and description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-itemize-enumerate
  '(itemize itemize-minus itemize-dot itemize-arrow
    enumerate enumerate-numeric enumerate-roman
    enumerate-Roman enumerate-alpha enumerate-Alpha))

(define list-description
  '(description description-compact description-aligned
    description-dash description-long))

(tm-define (inside-list?)
  (inside-which list-itemize-enumerate))

(tm-define (inside-description?)
  (inside-which list-description))

(tm-define (make-tmlist l)
  (make l)
  (make-item))

(tm-define (make-item)
  (if (not (make-return-after))
      (with l (inside-which (append list-itemize-enumerate list-description))
	(cond ((in? l list-itemize-enumerate) (make 'item))
	      ((in? l list-description) (make 'item*))))))

(tm-define (kbd-return)
  (:inside itemize itemize-minus itemize-dot itemize-arrow
	   enumerate enumerate-numeric enumerate-roman
	   enumerate-Roman enumerate-alpha enumerate-Alpha
	   description description-compact description-aligned
	   description-dash description-long)
  (make-item))

(tm-define (kbd-return)
  (:inside item*)
  (go-end-of 'item*))

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
  (if (not (make-return-after))
      (insert (list 'bibliography "bib" style file-name '(document "")))))
