
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml-expand.scm
;; DESCRIPTION : environment patch for expanding the document before conversion
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
	   '(TeXmacs TeX LaTeX hrule item
	     chapter-title section-title subsection-title subsubsection-title
	     paragraph-title subparagraph-title
	     itemize itemize-minus itemize-dot itemize-arrow
	     enumerate enumerate-numeric enumerate-roman
	     enumerate-Roman enumerate-alpha enumerate-Alpha
	     description description-compact description-dash
	     description-align description-long item*
	     strong em dfn code* samp kbd var abbr acronym
	     verbatim code tt
	     block block* tabular tabular*
	     doc-title-block equation* equation-lab equations-base
	     tmdoc-title tmdoc-flag tmdoc-license key
	     tmdoc-title*
	     tmdoc-title**
	     hyper-link tmdoc-copyright))))
