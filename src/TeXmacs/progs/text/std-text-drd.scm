
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-drd.scm
;; DESCRIPTION : data relation definitions for text mode
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text std-text-drd)
  (:use (utils edit variants)))

;; General groups

(define-group variant-tag
  (section-tag) (list-tag) (named-environment-tag) (figure-tag)
  (textual-tag))

(define-group similar-tag
  (section-tag) (list-tag) (named-environment-tag) (figure-tag)
  (textual-tag) (equation-tag))

(define-group numbered-tag
  (section-tag) (named-environment-tag) (figure-tag) (equation-tag))

;; Sections

(define-group section-tag
  part chapter appendix
  section subsection subsubsection
  paragraph subparagraph)

;; Lists

(define-group list-tag
  (itemize-tag) (enumerate-tag) (description-tag))

(define-group itemize-tag
  itemize itemize-minus itemize-dot itemize-arrow)

(define-group enumerate-tag
  enumerate enumerate-numeric enumerate-roman
  enumerate-Roman enumerate-alpha enumerate-Alpha)

(define-group description-tag
  description description-compact description-aligned
  description-dash description-long)

;; Theorems

(define-group named-environment-tag
  (theorem-tag) (definition-tag) (remark-tag) (exercise-tag))

(define-group theorem-tag
  theorem proposition lemma corollary conjecture)

(define-group definition-tag
  definition axiom notation)

(define-group remark-tag
  remark note example convention warning)

(define-group exercise-tag
  exercise problem)

;; Textual markup tags

(define-group textual-tag
  (strong-tag) (name-tag) (verbatim-tag))

(define-group strong-tag
  strong em dfn underline)

(define-group name-tag
  name person cite*)

(define-group verbatim-tag
  verbatim kbd code* var)

;; Figures and tables

(define-group figure-tag
  (small-figure-tag) (big-figure-tag))

(define-group small-figure-tag
  small-figure small-table)

(define-group big-figure-tag
  big-figure big-table)

;; Figures and tables

(define-group equation-tag
  equation eqnarray)
