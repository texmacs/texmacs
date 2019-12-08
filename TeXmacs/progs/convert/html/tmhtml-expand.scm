
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml-expand.scm
;; DESCRIPTION : environment patch for expanding the document before conversion
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "identity" macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-env-macro name)
  `(associate ,(symbol->string name)
	      (xmacro "x" (eval-args "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmhtml-env-patch)
  ;; FIXME: we should use the DRD here
  `(collection
    ,@(map tmhtml-env-macro
	   '(TeXmacs TeX LaTeX shown hrule item
	     chapter-title section-title subsection-title subsubsection-title
	     paragraph-title subparagraph-title
	     itemize itemize-minus itemize-dot itemize-arrow
	     enumerate enumerate-numeric enumerate-roman
	     enumerate-Roman enumerate-alpha enumerate-Alpha
	     description description-compact description-dash
	     description-align description-long description-paragraphs item*
	     strong em dfn code* samp kbd var abbr acronym
	     verbatim code tt
	     hidden-title doc-title-block
	     equation* equation-lab equations-base
             wide-float draw-over draw-under
             html-tag html-attr
	     html-div-style html-div-class html-style html-class
             html-javascript html-javascript-src html-video
	     web-title tmdoc-title tmdoc-flag tmdoc-license
	     tmdoc-title* tmdoc-title** tmdoc-copyright
	     hlink action hyper-link mouse-over-balloon mouse-over-balloon*))
    ;; FIXME: should apply 'filter_style' to the environment
    ;; in an appropriate way to avoid adding the primitives below
    ,@(map tmhtml-env-macro
           '(shrink-inline
             binom tbinom dbinom choose ontop
             tfrac dfrac cfrac
             bmod pmod pod))))
