
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-structure.scm
;; DESCRIPTION : Routines for structuring the sections and lists
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-structure)
  (:use (text text-drd)))

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

(define (tm/section-get-title-string-sub l indent?)
  (if (null? l) "no title"
      (with title (tm/section-get-title-string (car l) indent?)
	(if (!= title "no title") title
	    (tm/section-get-title-string-sub (cdr l) indent?)))))

(tm-define (texmacs->string x)
  (texmacs->code (verbatim-expand x) "cork"))

(tm-define (texmacs->title-string x)
  (downgrade-math-letters (texmacs->string x)))

(define (indent-prefix* sec)
  (cond ((in? sec '(chapter chapter*)) "")
        ((in? sec '(appendix appendix*)) "")
        ((in? sec '(section section*)) "   ")
        ((in? sec '(subsection subsection*)) "      ")
        ((in? sec '(subsubsection subsubsection*)) "         ")
        ((in? sec '(paragraph paragraph*)) "         ")
        ((in? sec '(subparagraph subparagraph*)) "         ")
        (else "")))

(define (indent-prefix sec)
  (with prefix (indent-prefix* sec)
    (if (and (short-style?) (string-starts? prefix "   "))
        (string-drop prefix 3)
        prefix)))

(tm-define (tm/section-get-title-string t indent?)
  (cond ((tm-atomic? t) "no title")
	((or (section-tag? (tm-car t)) (section*-tag? (tm-car t)))
         (with title (texmacs->title-string (tm-ref t 0))
           (if indent?
               (string-append (indent-prefix (tm-car t)) title)
               title)))
	((tree-is? (tm-car t) 'the-index) "Index")
	((tree-is? (tm-car t) 'the-glossary) "Glossary")
	((or (special-section-tag? (tm-car t))
	     (automatic-section-tag? (tm-car t)))
	 (upcase-first (string-replace (symbol->string (tm-car t)) "-" " ")))
	((tree-is? t 'concat)
	 (tm/section-get-title-string-sub (tree-children t) indent?))
        ((and (tm-func? t 'shared 3)
              (tm-func? (tm-ref t 2) 'document))
         (tm/section-get-title-string (tm-ref t 2 0) indent?))
	(else "no title")))

(define (tm/section-detect? t pred?)
  (cond ((tm-atomic? t) #f)
	((pred? (tm-car t)) #t)
	((tree-is? t 'concat)
	 (list-find (tree-children t)
		    (lambda (x) (tm/section-detect? x pred?))))
        ((and (tm-func? t 'shared 3)
              (tm-func? (tm-ref t 2) 'document))
         (tm/section-detect? (tm-ref t 2 0) pred?))
	(else #f)))

(define (tm/section-split l pred?)
  (list-split l (lambda (t) (tm/section-detect? t pred?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for the principal section structure (used for document parts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (short-style?)
  (!= (get-init-tree "sectional-short-style") (tree 'macro "false")))

(define (principal-section-predicate)
  (if (short-style?) short-principal-section-tag? long-principal-section-tag?))

(tm-define (principal-section? t)
  (tm/section-detect? t (principal-section-predicate)))

(define (list->subpart l nr)
  (with sec? (tm/section-detect? (car l) short-principal-section-tag?)
    `(show-part ,(number->string nr)
		(document ,@l)
		,(if sec? `(document ,(tm->stree (car l))) ""))))

(define (make-subparts l)
  (if (short-style?) l
      (with r (tm/section-split l short-principal-section-tag?)
	(map list->subpart r (... 1 (length r))))))

(define (list->document-part l nr)
  (with sec? (tm/section-detect? (car l) (principal-section-predicate))
    `(show-part ,(number->string nr)
		(document ,(car l) ,@(make-subparts (cdr l)))
		,(if sec? `(document ,(tm->stree (car l))) ""))))

(tm-define (principal-sections-to-document-parts l)
  (with r (tm/section-split l (principal-section-predicate))
    (map list->document-part r (... 1 (length r)))))

(define (principal-section-title-sub l)
  (cond ((null? l) "no title")
	((tm/section-detect? (car l) (principal-section-predicate))
	 (tm/section-get-title-string (car l) #f))
	(else (principal-section-title-sub (cdr l)))))

(tm-define (principal-section-title t)
  (principal-section-title-sub (tm-cdr t)))
