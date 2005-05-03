
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-edit)
  (:use (texmacs tools tm-circulate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-end-of-header-element)
  (if (inside? "address") (go-end-of "address"))
  (if (inside? "destination") (go-end-of "destination"))
  (if (inside? "cc") (go-end-of "cc"))
  (if (inside? "encl") (go-end-of "encl"))
  (go-end-line))

(tm-define (make-header l)
  (go-end-of-header-element)
  (if (!= (tree->stree (the-line)) "") (insert-return))
  (make l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sectional commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (inside-section?)
  (or (inside? "part")
      (inside? "part*")
      (inside? "chapter")
      (inside? "chapter*")
      (inside? "appendix")
      (inside? "section")
      (inside? "section*")
      (inside? "subsection")
      (inside? "subsection*")
      (inside? "subsubsection")
      (inside? "subsubsection*")
      (inside? "paragraph")
      (inside? "paragraph*")
      (inside? "subparagraph")
      (inside? "subparagraph*")))

(tm-define (make-section l)
  (if (not (make-return-after))
      (make l)))

(tm-define (make-unnamed-section l)
  (if (not (make-return-after))
      (make l)
      (make-return-before)))

(define (toggle-number-sub s)
  (with s* (string-append s "*")
    (cond ((inside? s) (variant-replace s s*))
	  ((inside? s*) (variant-replace s* s)))))

(tm-define (toggle-section-number)
  (for-each
   toggle-number-sub
   '("part" "chapter" "section" "subsection" "subsubsection"
     "paragraph" "subparagraph" "appendix"

     "theorem" "proposition" "lemma" "corollary"
     "axiom" "definition" "notation" "conjecture"
     "remark" "example" "note" "warning" "convention"
     "exercise" "problem"
     "small-figure" "big-figure" "small-table" "big-table")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists, enumerations and description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-itemize-enumerate
  '("itemize" "itemize-minus" "itemize-dot" "itemize-arrow"
    "enumerate" "enumerate-numeric" "enumerate-roman"
    "enumerate-Roman" "enumerate-alpha" "enumerate-Alpha"))

(define list-description
  '("description" "description-compact" "description-aligned"
    "description-dash" "description-long"))

(tm-define (inside-list?)
  (!= (inside-which list-itemize-enumerate) ""))

(tm-define (inside-description?)
  (!= (inside-which list-description) ""))

(tm-define (make-tmlist l)
  (make l)
  (make-item))

(tm-define (make-item)
  (if (not (make-return-after))
      (with l (inside-which (append list-itemize-enumerate list-description))
	(cond ((in? l list-itemize-enumerate) (make 'item))
	      ((in? l list-description) (make 'item*))))))

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
