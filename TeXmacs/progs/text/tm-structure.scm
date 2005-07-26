
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-structure.scm
;; DESCRIPTION : Routines for structuring the sections and lists
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text tm-structure)
  (:use (text std-text-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra subroutines on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-split l pred?)
  "Split @l into as many sublists as possible starting with matching items"
  (cond ((null? l) l)
	((null? (cdr l)) (list l))
	(else
	 (with parts (list-split (cdr l) pred?)
	   (if (pred? (caar parts))
	       (cons (list (car l)) parts)
	       (cons (cons (car l) (car parts)) (cdr parts)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detecting sections inside paragraph lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm/section-get-title-string-sub l)
  (if (null? l) "no title"
      (with title (tm/section-get-title-string (car l))
	(if (!= title "no title") title
	    (tm/section-get-title-string-sub (cdr l))))))

(tm-define (tm/section-get-title-string t)
  (cond ((tm-atomic? t) "no title")
	((or (section-tag? (tm-car t)) (section*-tag? (tm-car t)))
	 (plugin-math-input
	  (list 'tuple "default" (tree->stree (tm->tree (tm-ref t 0))))))
	((tree-is? (tm-car t) 'the-index) "Index")
	((tree-is? (tm-car t) 'the-glossary) "Glossary")
	((or (special-section-tag? (tm-car t))
	     (automatic-section-tag? (tm-car t)))
	 (upcase-first (string-replace (symbol->string (tm-car t)) "-" " ")))
	((tree-is? t 'concat)
	 (tm/section-get-title-string-sub (tree-children t)))
	(else "no title")))

(define (tm/section-detect? t pred?)
  (cond ((tm-atomic? t) #f)
	((pred? (tm-car t)) #t)
	((tree-is? t 'concat)
	 (list-find (tree-children t)
		    (lambda (x) (tm/section-detect? x pred?))))
	(else #f)))

(define (tm/section-split l pred?)
  (list-split l (lambda (t) (tm/section-detect? t pred?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for the principal section structure (used for document parts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (principal-section-predicate)
  (if (!= (get-init-tree "sectional-short-style") (tree 'macro "false"))
      short-principal-section-tag?
      long-principal-section-tag?))

(tm-define (principal-section? t)
  (tm/section-detect? t (principal-section-predicate)))

(define (list->document-part l)
  (if (tm/section-detect? (car l) (principal-section-predicate))
      `(show-part "auto" (document ,@l) (document ,(car l)))
      `(show-part "front matter" (document ,@l) "")))

(tm-define (principal-sections-to-document-parts l)
  (with r (tm/section-split l (principal-section-predicate))
    (map list->document-part r)))

(define (principal-section-title-sub l)
  (cond ((null? l) "no title")
	((tm/section-detect? (car l) (principal-section-predicate))
	 (tm/section-get-title-string (car l)))
	(else (principal-section-title-sub (cdr l)))))

(tm-define (principal-section-title t)
  (principal-section-title-sub (tm-cdr t)))
