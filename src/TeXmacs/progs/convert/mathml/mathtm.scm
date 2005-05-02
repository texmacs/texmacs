
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathtm.scm
;; DESCRIPTION : conversion of MathML trees to TeXmacs trees
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MathML markup found by inspection of Macaulay2
;; String -- <mtext>
;; Symbol -- <mi>
;; MatrixExpression -- <mrow> <mo> <mtable> <mtr> <mtd>
;; operators -- <mo> <mfrac>
;; literals -- <mn> (0 or 1)
;; Power -- <msup>
;; ZZ -- <mn>ZZ</mn>
;; RR -- <mn>RR</mn>
;; QQ -- <mfrac>...
;; Inifinite -- <mrow> <mo> <mi> &infin;

;; -- Special symbols
;; symbol ii => "&ii;" &ImaginaryI;
;; booleans &true; &false;
;; -- mo contents
;; ( ) - + * &InvisibleTimes; &it;

;; Extra markup used in the texmacs interface
;; <mrow> <mi> <mo>:</mo> <mo>=</mo> <mtext>

;; Extra symbols to support
;; &#2124; -- bbb-Z
;; &#211A; -- bbb-Q
;; &#211D; -- bbb-R

;; -- MathML notes --
;;
;; . Linking is defined by XLink.
;; . mtr and mtd may be inferred for compat with MathML1
;; . transfinite symbols are U+2135-U+2138
;; . how to render empty tokens?
;; . how to render whitespace in tokens?

;; . mathvariant affect comparison of chars corresponding to SMP conterparts

;; . slanting rules of <mi> match rules of texmacs
;; . <mi>...</mi> might be rendered as ldots, check if it is a MathML char
;; . only a very reduced subset of <mn> can be natively supported
;; . <mo> containing text cannot be supported
;; . nesting of <mrow> can denote range of delimiters in <mo>
;; . texmacs cannot do \widearrow (script under extensible arrow)
;; . <mo> containing text may be rendered as nested text delimited by spaces
;;   (see 3.2.6.4)

;; It is non-trivial to support the correct semantics of mrows w.r.t. to
;; delimiters and indices. That may be done by producing a pair of invisible
;; big delimiters for every mrow. Extensible <mo> will then be rendered as big
;; separators, and indices will correctly be attached to a block.

;; !!! Check semantic of mathvariant on multiple symbols !!!

(texmacs-module (convert mathml mathtm)
  (:use (convert tools tmtable) (convert tools sxml) (convert tools xmltm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safe-cdr l)
  (if (null? l) '() (cdr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smathml->tmtable env x)
  ;; Simplistic parser for mathml table.
  ;; Properly infer implied <mtr> and <mtd>.
  (if (!= 'mtable (sxml-name x))
      (error "Parameter is not a mtable."))
  (let ((a (sxml-attr-list x))
	(c (sxml-content x)))
    (tmtable (list (tmformat-table "cell-halign" "c"))
	     (map (lambda (x) (mathtm-table-row env x)) c))))

(define (mathtm-table-row env x)
  ;; Given a mtable item, produce a list of cell contents, inferring <mtr> for
  ;; single column rows if necessary.
  (let ((name (sxml-name x))
	(content (sxml-filter-element-content (sxml-content x))))
    (cond ((== 'mtr name)
	   (map (lambda (x) (mathtm-table-cell env x)) content))
	  ((== 'mlabeledtr name)
	   (map (lambda (x) (mathtm-table-cell env x)) (safe-cdr content)))
	  (else (list mathtm-table-cell env x)))))

(define (mathtm-table-cell env x)
  ;; Given an mrow item, produce a cell content, inferring <mtd> or <mrow> if
  ;; necessary.
  (let ((name (sxml-name x))
	(content (sxml-filter-element-content (sxml-content x))))
    (mathtm-as-serial env
		      (if (== 'mtd name)
			  (cond ((null? content) '(mrow))
				((null? (cdr content)) (car content))
				(else `(mrow ,@content)))
			  x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-math env a c)
  ;; TODO: support display modes
  `((with "mode" "math" ,(mathtm-args-serial env c))))

(define (mathtm-mo env a c)
  ;; TODO: something smart for unbalanced parenthesis
  ;;       can be done in the method for mrow
  ;;       additional plumbing is required for implied mrows
  (cond ((== c '("(")) '((left "(")))
	((== c '(")")) '((right ")")))
	(else (list (mathtm-args-serial env c)))))

(define (mathtm-mtext env a c)
  ;; TODO: be smarter with spaces
  `((with "mode" "text"
      ,(mathtm-args-serial env c))))

(define (mathtm-mfrac env a c)
  ;; TODO: produce <merror> if arity is incorrect
  `((frac
     ,(mathtm-as-serial env (first c))
     ,(mathtm-as-serial env (second c)))))

(define (length=1? l) (and (nnull? l) (null (cdr l))))

(define (mathtm-msub env a c)
  ;; TODO: produce <merror> if arity is incorrect
  ;; NOTE: invisible delimiters are required when 'base' is complex.
  (let ((base (mathtm env (first c)))
	(sub (mathtm-as-serial env (second c))))
    (list (mathtm-serial
	   env (if (length=1? base)
		   `(,@base (rsub ,sub))
		   `((left ".") ,@base (right ".") (rsub ,sub)))))))

(define (mathtm-msup env a c)
  ;; TODO: produce <merror> if arity is incorrect
  ;; NOTE: invisible delimiters are required when 'base' is complex.
  ;; TODO: maybe consolidate with mathtm-sub?
  (let ((base (mathtm env (first c)))
	(sup (mathtm-as-serial env (second c))))
    (list (mathtm-serial
	   env (if (length=1? base)
		   `(,@base (rsup ,sup))
		   `((left ".") ,@base (right ".") (rsup ,sup)))))))

(define (mathtm-mtable env a c)
  (list (tmtable->tm (smathml->tmtable env `(mtable (@ ,a) ,@c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-string env s)
  ;; FIXME: use translators or parser for this!!!
  ;; TODO: learn when the trailing ';' is optional
  (cond ((assoc s '(("&ii;" . "<mathi>")
		    ("&ImaginaryI;" . "<mathi>")
		    ("&true;" . "true")
		    ("&false;" . "false")
		    ("&InvisibleTimes;" . "*")
		    ("&it;" . "*")
		    ("*" . "*")
		    ("&ApplyFunction;" . " ")
		    ("&RightArrow;" . "<rightarrow>")
		    ("&infin;" . "<infty>")
		    ("&Copf;" . "<bbb-C>")
		    ("&Qopf;" . "<bbb-Q>")
		    ("&Zopf;" . "<bbb-Z>")
		    ("&Ropf;" . "<bbb-R>")))
	 => (lambda (p) (cdr p)))
	(else (xmltm-text s))))

(define (mathtm-drop env a c) '())

(define (mathtm-pass env a c)
  ;; TODO: consolidate with htmltm-pass
  (let ((l (mathtm-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate a 'id (mathtm-serial env l))))))

(define (mathtm-args env l)
  ;; TODO: consolidate with htmltm-args
  (append-map (lambda (x) (mathtm env x)) l))

(define (mathtm-args-serial env l)
  ;; FIXME: mathml must not be affected by xml:space (html <pre> mode)
  (mathtm-serial env (mathtm-args env l)))

(define (mathtm env t)
  (sxml-dispatch (lambda (env t) (list (mathtm-string env t)))
		 mathtm-pass env t))

(tm-define mathtm-as-serial
  ;; TODO: replace #f by 'environment' object, see htmltm-as-serial
  (case-lambda
    ((t) (mathtm-as-serial #f t))
    ((env t) (mathtm-serial env (mathtm env t)))))

(drd-dispatcher mathtm-methods%
  ;;; Interface
  (math (mathtm-handler :element mathtm-math))
  ;;; Presentation
  ;; Token
  ;; presentation tokens contain CDATA, MathML entities, align marks, or glyphs
  (mi (mathtm-handler :mixed mathtm-pass))
  (mn (mathtm-handler :mixed mathtm-pass))
  (mo (mathtm-handler :mixed mathtm-mo))
  (mtext (mathtm-handler :mixed mathtm-mtext))
  (mspace (mathtm-handler :mixed mathtm-drop))
  (ms (mathtm-handler :mixed mathtm-mtext))
  (mglyph (mathtm-handler :empty mathtm-drop))
  ;; General layout
  (mrow (mathtm-handler :element mathtm-pass))
  (mfrac (mathtm-handler :element mathtm-mfrac))
  (msqrt (mathtm-handler :element mathtm-pass))
  (mroot (mathtm-handler :element mathtm-pass))
  (mstyle (mathtm-handler :element mathtm-pass))
  (merror (mathtm-handler :element mathtm-pass))
  (mpadded (mathtm-handler :element mathtm-pass))
  (mphantom (mathtm-handler :element mathtm-pass))
  (mfenced (mathtm-handler :element mathtm-pass))
  (menclose (mathtm-handler :element mathtm-pass))
  ;; Script and limits
  (msub (mathtm-handler :element mathtm-msub))
  (msup (mathtm-handler :element mathtm-msup))
  (msubsup (mathtm-handler :element mathtm-pass))
  (munder (mathtm-handler :element mathtm-pass))
  (mover (mathtm-handler :element mathtm-pass))
  (munderover (mathtm-handler :element mathtm-pass))
  (mmultiscripts (mathtm-handler :element mathtm-pass))
  ;; Tables
  (mtable (mathtm-handler :element mathtm-mtable))
  (mtr (mathtm-handler :element mathtm-pass))
  (mlabeledtr (mathtm-handler :element mathtm-pass))
  (mtd (mathtm-handler :element mathtm-pass))
  ;; Actions
  (maction (mathtm-handler :element mathtm-pass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mathml->tree s)
  (:synopsis "Convert the MathML @s into a document fragment.")
  (mathtm-as-serial (parse-xml s)))
