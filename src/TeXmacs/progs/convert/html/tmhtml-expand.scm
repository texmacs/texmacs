
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

(texmacs-module (convert html tmhtml-expand)
  (:export tmhtml-env-patch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "identity" macros and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-env-macro-0 name)
  `(associate ,(symbol->string name)
	      (macro (hold (,name)))))

(define (tmhtml-env-macro-1 name)
  `(associate ,(symbol->string name)
	      (macro "x"
		     (hold (,name (release (arg "x")))))))

(define (tmhtml-env-macro-2 name)
  `(associate ,(symbol->string name)
	      (macro "x" "y"
		     (hold (,name (release (arg "x"))
				  (release (arg "y")))))))

(define (tmhtml-env-macro-3 name)
  `(associate ,(symbol->string name)
	      (macro "x" "y" "z"
		     (hold (,name (release (arg "x"))
				  (release (arg "y"))
				  (release (arg "z")))))))

(define (tmhtml-env-func name)
  `(associate ,(symbol->string name)
	      (func "a" "b" "c" "d" "e"
		    (case
		      (unequal (apply "e") "")
		      (hold (apply ,name
				   (release (apply "a"))
				   (release (apply "b"))
				   (release (apply "c"))
				   (release (apply "d"))
				   (release (apply "e"))))
		      (unequal (apply "d") "")
		      (hold (apply ,name
				   (release (apply "a"))
				   (release (apply "b"))
				   (release (apply "c"))
				   (release (apply "d"))))
		      (unequal (apply "c") "")
		      (hold (apply ,name
				   (release (apply "a"))
				   (release (apply "b"))
				   (release (apply "c"))))
		      (unequal (apply "b") "")
		      (hold (apply ,name
				   (release (apply "a"))
				   (release (apply "b"))))
		      (unequal (apply "a") "")
		      (hold (apply ,name
				   (release (apply "a"))))
		      (hold (apply ,name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-env-patch)
  ;; FIXME: we should use the DRD here
  `(collection
    ,@(map tmhtml-env-macro-0 '(TeXmacs TeX LaTeX item))
    ,@(map tmhtml-env-macro-1
	   '(chapter* section* subsection* subsubsection*
	     paragraph* subparagraph*
	     itemize itemize-minus itemize-dot itemize-arrow
	     enumerate enumerate-numeric enumerate-roman
	     enumerate-Roman enumerate-alpha enumerate-Alpha
	     description description-compact description-dash
	     description-align description-long item*
	     strong em dfn code* samp kbd var abbr acronym
	     verbatim code tt
	     block block* tabular tabular*
	     tmdoc-title tmdoc-flag tmdoc-license key))
    ,@(map tmhtml-env-macro-2 '(tmdoc-title*))
    ,@(map tmhtml-env-macro-3 '(tmdoc-title**))
    ,@(map tmhtml-env-func '(hyper-link tmdoc-copyright))))
