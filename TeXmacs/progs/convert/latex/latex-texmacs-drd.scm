
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-texmacs-drd.scm
;; DESCRIPTION : TeXmacs extensions to LaTeX
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-texmacs-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catcode tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table iso-latin-catcodes%
  ("à" "\\`a")
  ("À" "\\`A")
  ("á" "\\'a")
  ("Á" "\\'A")
  ("ä" "\\\"a")
  ("Ä" "\\\"A")
  ("â" "\\^a")
  ("Â" "\\^A")
  ("å" "{\\aa}")
  ("Å" "{\\AA}")
  ("ç" "\\c{c}")
  ("Ç" "\\c{C}")
  ("è" "\\`e")
  ("È" "\\`E")
  ("é" "\\'e")
  ("É" "\\'E")
  ("ë" "\\\"e")
  ("Ë" "\\\"E")
  ("ê" "\\^e")
  ("Ê" "\\^E")
  ("ì" "\\`{\\i}")
  ("Ì" "\\`{\\I}")
  ("í" "\\'{\\i}")
  ("Í" "\\'{\\I}")
  ("ï" "\\\"{\\i}")
  ("Ï" "\\\"{\\I}")
  ("î" "\\^{\\i}")
  ("Î" "\\^{\\I}")
  ("ò" "\\`o")
  ("Ò" "\\`O")
  ("ó" "\\'o")
  ("Ó" "\\'O")
  ("ö" "\\\"o")
  ("Ö" "\\\"O")
  ("ô" "\\^o")
  ("Ô" "\\^O")
  ("ù" "\\`u")
  ("Ù" "\\`U")
  ("ú" "\\'u")
  ("Ú" "\\'U")
  ("ü" "\\\"u")
  ("Ü" "\\\"U")
  ("û" "\\^u")
  ("Û" "\\^U")
  ("ý" "\\'y")
  ("Ý" "\\'Y")
  ("ÿ" "\\\"y")
  ("˜" "\\\"Y")
  ("½" "!`")
  ("¾" "?`")
  ("ß" "{\\ss}"))

(drd-table cyrillic-catcodes%
  ("À" "\\CYRA")
  ("à" "\\cyra")
  ("Á" "\\CYRB")
  ("á" "\\cyrb")
  ("Â" "\\CYRV")
  ("â" "\\cyrv")
  ("Ã" "\\CYRG")
  ("ã" "\\cyrg")
  ("Ä" "\\CYRD")
  ("ä" "\\cyrd")
  ("Å" "\\CYRE")
  ("å" "\\cyre")
  ("Æ" "\\CYRZH")
  ("æ" "\\cyrzh")
  ("Ç" "\\CYRZ")
  ("ç" "\\cyrz")
  ("È" "\\CYRI")
  ("è" "\\cyri")
  ("É" "\\CYRISHRT")
  ("é" "\\cyrishrt")
  ("Ê" "\\CYRK")
  ("ê" "\\cyrk")
  ("Ë" "\\CYRL")
  ("ë" "\\cyrl")
  ("Ì" "\\CYRM")
  ("ì" "\\cyrm")
  ("Í" "\\CYRN")
  ("í" "\\cyrn")
  ("Î" "\\CYRO")
  ("î" "\\cyro")
  ("Ï" "\\CYRP")
  ("ï" "\\cyrp")
  ("Ð" "\\CYRR")
  ("ð" "\\cyrr")
  ("Ñ" "\\CYRS")
  ("ñ" "\\cyrs")
  ("Ò" "\\CYRT")
  ("ò" "\\cyrt")
  ("Ó" "\\CYRU")
  ("ó" "\\cyru")
  ("Ô" "\\CYRF")
  ("ô" "\\cyrf")
  ("Õ" "\\CYRH")
  ("õ" "\\cyrh")
  ("Ö" "\\CYRC")
  ("ö" "\\cyrc")
  ("×" "\\CYRCH")
  ("÷" "\\cyrch")
  ("Ø" "\\CYRSH")
  ("ø" "\\cyrsh")
  ("Ù" "\\CYRSHCH")
  ("ù" "\\cyrshch")
  ("Ú" "\\CYRHRDSN")
  ("ú" "\\cyrhrdsn")
  ("Û" "\\CYRERY")
  ("û" "\\cyrery")
  ("Ü" "\\CYRSFTSN")
  ("ü" "\\cyrsftsn")
  ("Ý" "\\CYREREV")
  ("ý" "\\cyrerev")
  ("Þ" "\\CYRYU")
  ("þ" "\\cyryu")
  ("ß" "\\CYRYA")
  ("ÿ" "\\cyrya")
  ("œ" "\\CYRYO")
  ("¼" "\\cyryo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catcode substitutions and definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define latex-language "english")
(define latex-cyrillic-catcode? #f)
(define latex-style "generic")
(define latex-style-hyp 'generic-style%)

(tm-define (latex-set-language lan)
  (set! latex-language lan)
  (set! latex-cyrillic-catcode?
	(in? lan '("bulgarian" "russian" "ukrainian"))))

(tm-define (latex-set-style sty)
  (set! latex-style sty)
  (set! latex-style-hyp (string->symbol (string-append sty "-style%"))))

(tm-define (latex-catcode c)
  (with s (list->string (list c))
    (or (if latex-cyrillic-catcode?
	    (drd-ref cyrillic-catcodes% s)
	    (drd-ref iso-latin-catcodes% s))
	s)))

(define latex-catcode-table (make-ahash-table))

(define (latex-catcode-defs-char c)
  (let* ((s (list->string (list c)))
	 (r (latex-catcode c)))
    (if (!= r s) (ahash-set! latex-catcode-table s r))))

(define (latex-catcode-defs-sub doc)
  (cond ((string? doc) (for-each latex-catcode-defs-char (string->list doc)))
	((list? doc) (for-each latex-catcode-defs-sub doc))))

(define (latex-catcode-def key im)
  (string-append "\\catcode`\\" key "=\\active \\def" key "{" im "}\n"))

(tm-define (latex-catcode-defs doc)
  (set! latex-catcode-table (make-ahash-table))
  (latex-catcode-defs-sub doc)
  (let* ((l1 (ahash-table->list latex-catcode-table))
	 (l2 (list-sort l1 (lambda (x y) (string<=? (car x) (car y)))))
	 (l3 (map (lambda (x) (latex-catcode-def (car x) (cdr x))) l2)))
    (apply string-append l3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-symbol%
  ;; arrows with limits
  (leftarrowlim "\\mathop{\\leftarrow}\\limits")
  (rightarrowlim "\\mathop{\\rightarrow}\\limits")
  (leftrightarrowlim "\\mathop{\\leftrightarrow}\\limits")
  (mapstolim "\\mathop{\\mapsto}\\limits")
  (longleftarrowlim "\\mathop{\\longleftarrow}\\limits")
  (longrightarrowlim "\\mathop{\\longrightarrow}\\limits")
  (longleftrightarrowlim "\\mathop{\\longleftrightarrow}\\limits")
  (longmapstolim "\\mathop{\\longmapsto}\\limits")
  (equallim "\\mathop{=}\\limits")
  (longequallim "\\mathop{\\longequal}\\limits")
  (Leftarrowlim "\\mathop{\\leftarrow}\\limits")
  (Rightarrowlim "\\mathop{\\rightarrow}\\limits")
  (Leftrightarrowlim "\\mathop{\\leftrightarrow}\\limits")
  (Longleftarrowlim "\\mathop{\\longleftarrow}\\limits")
  (Longrightarrowlim "\\mathop{\\longrightarrow}\\limits")
  (Longleftrightarrowlim "\\mathop{\\longleftrightarrow}\\limits")
   
  ;; asymptotic relations by Joris
  (nasymp "\\not\\asymp")
  (asympasymp "\\asymp\\!\\!\\!-")
  (nasympasymp "\\not\\asymp\\!\\!\\!-")
  (simsim "\\approx\\!\\!\\!-")
  (nsimsim "\\not\\approx\\!\\!\\!-")
  (npreccurlyeq "\\not\\preccurlyeq")
  (precprec "\\prec\\!\\!\\!\\prec")
  (precpreceq "\\preceq\\!\\!\\!\\preceq")
  (precprecprec "\\prec\\!\\!\\!\\prec\\!\\!\\!\\prec")
  (precprecpreceq "\\preceq\\!\\!\\!\\preceq\\!\\!\\!\\preceq")
  (succsucc "\\succ\\!\\!\\!\\succ")
  (succsucceq "\\succeq\\!\\!\\!\\succeq")
  (succsuccsucc "\\succ\\!\\!\\!\\succ\\!\\!\\!\\succ")
  (succsuccsucceq "\\succeq\\!\\!\\!\\succeq\\!\\!\\!\\succeq")
  (lleq "\\leq\\negmedspace\\negmedspace\\leq")
  (llleq "\\leq\\negmedspace\\negmedspace\\leq\\negmedspace\\negmedspace\\leq")
  (ggeq "\\geq\\negmedspace\\negmedspace\\geq")
  (gggeq "\\geq\\negmedspace\\negmedspace\\geq\\negmedspace\\negmedspace\\geq")

  ;; extra literal symbols
  (Backepsilon "{\\mbox{\\rotatebox[origin=c]{180}{E}}}")
  (mho "\\renewcommand{\\mho}{\\mbox{\\rotatebox[origin=c]{180}{$\\omega$}}}")
  (Mho "{\\mbox{\\rotatebox[origin=c]{180}{$\\Omega$}}}")
  (mathd "\\mathrm{d}")
  (mathe "\\mathrm{e}")
  (mathi "\\mathrm{i}")
  (mathpi "\\pi")
  (Alpha "\\mathrm{A}")
  (Beta "\\mathrm{B}")
  (Epsilon "\\mathrm{E}")
  (Eta "\\mathrm{H}")
  (Iota "\\mathrm{I}")
  (Kappa "\\mathrm{K}")
  (Mu "\\mathrm{M}")
  (Nu "\\mathrm{N}")
  (Omicron "\\mathrm{O}")
  (Chi "\\mathrm{X}")
  (Rho "\\mathrm{P}")
  (Tau "\\mathrm{T}")
  (Zeta "\\mathrm{Z}")

  ;; other extra symbols
  (bigintwl "\\int")
  (bigointwl "\\oint")
  (bignone "")
  (asterisk "*")
  (nonesep "")
  (nin "\\not\\in")
  (nequiv "\\not\\equiv")
  (dotamalg "\\mathaccent95{\\amalg}")
  (dottimes "\\mathaccent95{\\times}")
  (dotoplus "\\mathaccent95{\\oplus}")
  (dototimes "\\mathaccent95{\\otimes}")
  (longequal "{=\\!\\!=}")
  (longhookrightarrow "{\\lhook\\joinrel\\relbar\\joinrel\\rightarrow}")
  (longhookleftarrow "{\\leftarrow\\joinrel\\relbar\\joinrel\\rhook}")
  (triangleup "\\triangle")
  (precdot "{\\prec\\hspace{-0.6em}\\cdot}\\;\\,")
  (preceqdot "{\\preccurlyeq\\hspace{-0.6em}\\cdot}\\;\\,")
  (join "\\Join")
  (um "-")
  (assign ":=")
  (plusassign "+\\!\\!=")
  (minusassign "-\\!\\!=")
  (timesassign "\times\\!\\!=")
  (overassign "/\\!\\!=")
  (udots "{\\mathinner{\\mskip1mu\\raise1pt\\vbox{\\kern7pt\\hbox{.}}\\mskip2mu\\raise4pt\\hbox{.}\\mskip2mu\\raise7pt\\hbox{.}\\mskip1mu}}"))

(drd-rules
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-symbol% 'x 'body))
  ((latex-texmacs-arity% 'x 0) (latex-texmacs-symbol% 'x 'body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-nullary%
  (tmunsc "\\_")
  (emdash "---")
  (tmat "\\symbol{\"40}")
  (tmbsl "\\ensuremath{\\backslash}")
  (tmdummy "$\\mbox{}$")
  (TeXmacs "T\\kern-.1667em\\lower.5ex\\hbox{E}\\kern-.125emX\\kern-.1em\\lower.5ex\\hbox{\\textsc{m\\kern-.05ema\\kern-.125emc\\kern-.05ems}}")
  (madebyTeXmacs (footnote (!recurse (withTeXmacstext))))
  (withTeXmacstext
   (!literal (!translate "This document has been produced using") " "
	     (!recurse (TeXmacs)) " (" (!translate "see") " "
	     (texttt "http://www.texmacs.org") ")"))
  (scheme "{\\sc Scheme}")
  (pari "{\\sc Pari}"))

(drd-table latex-texmacs-unary%
  (tmmathbf (ensuremath (boldsymbol 1)))
  (tmop (ensuremath (operatorname 1)))
  (tmstrong (textbf 1))
  (tmem (!group "\\em " 1 "\\/"))
  (tmtt (texttt 1))
  (tmname (textsc 1))
  (tmsamp (textsf 1))
  (tmabbr 1)
  (tmdfn (textbf 1))
  (tmkbd (texttt 1))
  (tmvar (texttt 1))
  (tmacronym (textsc 1))
  (tmperson (textsc 1))
  (tmscript (text (scriptsize (!math 1))))
  (tmdef 1)
  (dueto (textup (textbf (!literal "(" 1 ") "))))
  (op 1)
  (email (!group (textit (!translate "Email:")) " " (texttt 1)))
  (homepage (!group (textit (!translate "Web:")) " "(texttt 1)))
  (keywords (!group (textbf (!translate "Keywords:")) " " 1))
  (AMSclass (!group (textbf (!translate "A.M.S. subject classification:"))
		    " " 1)))

(drd-table latex-texmacs-binary%
  (tmhlink (!group "\\color{blue} " 1))
  (tmaction (!group "\\color{blue} " 1))
  (subindex (index (!literal 1 "!" 2))))

(drd-table latex-texmacs-ternary%
  (subsubindex (index (!literal 1 "!" 2 "!" 3)))
  (tmref 1))

(drd-table latex-texmacs-tetrary%
  (subsubindex (index (!literal 1 "!" 2 "!" 3 "!" 4))))

(drd-rules
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-nullary% 'x 'body))
  ((latex-texmacs-arity% 'x 0) (latex-texmacs-nullary% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-unary% 'x 'body))
  ((latex-texmacs-arity% 'x 1) (latex-texmacs-unary% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-binary% 'x 'body))
  ((latex-texmacs-arity% 'x 2) (latex-texmacs-binary% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-ternary% 'x 'body))
  ((latex-texmacs-arity% 'x 3) (latex-texmacs-ternary% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-tetrary% 'x 'body))
  ((latex-texmacs-arity% 'x 4) (latex-texmacs-tetrary% 'x 'body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-environment%
  ("proof"
   (!literal (noindent) (textbf (!literal (!translate "Proof") "\\ "))
	     ---
	     (hspace* (fill)) (!math (Box)) (medskip)))
  ("proof*"
   (!literal (noindent) (textbf (!literal 1 "\\ "))
	     ---
	     (hspace* (fill)) (!math (Box)) (medskip)))
  ("tmparmod"
   ((!begin "list" "" (!literal "\\setlength{\\topsep}{0pt}"
				"\\setlength{\\leftmargin}{" 1 "}"
				"\\setlength{\\rightmargin}{" 2 "}"
				"\\setlength{\\parindent}{" 3 "}"
				"\\setlength{\\listparindent}{\\parindent}"
				"\\setlength{\\itemindent}{\\parindent}"
				"\\setlength{\\parsep}{\\parskip}"))
    (!literal "\\item[]"
	      ---))))

(drd-table latex-texmacs-env-arity%
  ("proof" 0)
  ("proof*" 1)
  ("tmparmod" 3))

(define-macro (latex-texmacs-itemize env lab)
  `(begin
     (drd-table latex-texmacs-environment%
       (,env
	((!begin "itemize")
	 (!literal "\\renewcommand{\\labelitemi}{" ,lab "}"
		   "\\renewcommand{\\labelitemii}{" ,lab "}"
		   "\\renewcommand{\\labelitemiii}{" ,lab "}"
		   "\\renewcommand{\\labelitemiv}{" ,lab "}"
		   ---
		   "\\end{itemize}"))))
     (drd-table latex-texmacs-env-arity%
       (,env 0))))

(define-macro (latex-texmacs-enumerate env lab)
  `(begin
     (drd-table latex-texmacs-environment%
       (,env ((!begin "enumerate" (!option ,lab)) ---)))
     (drd-table latex-texmacs-env-arity%
       (,env 0))))

(latex-texmacs-itemize "itemizeminus" "$-$")
(latex-texmacs-itemize "itemizedot" "$\\bullet$")
(latex-texmacs-itemize "itemizearrow" "$\\rightarrow$")
(latex-texmacs-enumerate "enumeratenumeric" "1.")
(latex-texmacs-enumerate "enumerateroman" "i.")
(latex-texmacs-enumerate "enumerateromancap" "I.")
(latex-texmacs-enumerate "enumeratealpha" "a{\\textup{)}}")
(latex-texmacs-enumerate "enumeratealphacap" "A.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra preamble definitions which are needed to export certain macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-preamble%
  (color
   (!literal
    "\\definecolor{grey}{rgb}{0.75,0.75,0.75}\n"
    "\\definecolor{orange}{rgb}{1.0,0.5,0.5}\n"
    "\\definecolor{brown}{rgb}{0.5,0.25,0.0}\n"
    "\\definecolor{pink}{rgb}{1.0,0.5,0.5}\n"))
  (tmfloat
   (!literal
    "\\newcommand{\\tmfloatcontents}{}\n"
    "\\newlength{\\tmfloatwidth}\n"
    "\\newcommand{\\tmfloat}[5]{\n"
    "  \\renewcommand{\\tmfloatcontents}{#4}\n"
    "  \\setlength{\\tmfloatwidth}{\\widthof{\\tmfloatcontents}+1in}\n"
    "  \\ifthenelse{\\equal{#2}{small}}\n"
    "    {\\ifthenelse{\\lengthtest{\\tmfloatwidth > \\linewidth}}\n"
    "      {\\setlength{\\tmfloatwidth}{\\linewidth}}{}}\n"
    "    {\\setlength{\\tmfloatwidth}{\\linewidth}}"
    "  \\begin{minipage}[#1]{\\tmfloatwidth}\n"
    "    \\begin{center}\n"
    "      \\tmfloatcontents\n"
    "      \\captionof{#3}{#5}\n"
    "    \\end{center}\n"
    "  \\end{minipage}}")))

(define-macro (latex-texmacs-theorem prim name)
  `(drd-table latex-texmacs-env-preamble%
     (,prim (!literal "\\newtheorem{" ,prim "}{" (!translate ,name) "}\n"))))

(define-macro (latex-texmacs-remark prim name)
  `(drd-table latex-texmacs-env-preamble%
     (,prim (!literal "{" (!recurse (theorembodyfont "\\rmfamily"))
		      "\\newtheorem{" ,prim "}{" (!translate ,name) "}}\n"))))

(define-macro (latex-texmacs-exercise prim name)
  `(drd-table latex-texmacs-env-preamble%
     (,prim (!literal "{" (!recurse (theorembodyfont "\\rmfamily\\small"))
		      "\\newtheorem{" ,prim "}{" (!translate ,name) "}}\n"))))

(latex-texmacs-theorem "theorem" "Theorem")
(latex-texmacs-theorem "proposition" "Proposition")
(latex-texmacs-theorem "lemma" "Lemma")
(latex-texmacs-theorem "corollary" "Corollary")
(latex-texmacs-theorem "lemma" "Lemma")
(latex-texmacs-theorem "axiom" "Axiom")
(latex-texmacs-theorem "lemma" "Lemma")
(latex-texmacs-theorem "definition" "Definition")
(latex-texmacs-theorem "notation" "Notation")
(latex-texmacs-remark "remark" "Remark")
(latex-texmacs-remark "note" "Note")
(latex-texmacs-remark "example" "Example")
(latex-texmacs-remark "convention" "Convention")
(latex-texmacs-remark "warning" "Warning")
(latex-texmacs-exercise "exercise" "Exercise")
(latex-texmacs-exercise "problem" "Problem")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style-dependent macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-nullary%
  (appendix "" letter-style%))

(define-macro (latex-texmacs-section name inside . conds)
  `(drd-table latex-texmacs-unary%
     (,name (!literal (medskip) (bigskip) "\n\n" (noindent) (textbf ,inside))
	    ,@conds)))

(define-macro (latex-texmacs-paragraph name inside . conds)
  `(drd-table latex-texmacs-unary%
     (,name (!literal (smallskip) "\n\n" (noindent) (textbf ,inside))
	    ,@conds)))

(latex-texmacs-section chapter (!literal "\\huge " 1) article-style%)
(latex-texmacs-section chapter (!literal "\\huge " 1) letter-style%)
(latex-texmacs-section section (!literal "\\LARGE " 1) letter-style%)
(latex-texmacs-section subsection (!literal "\\Large " 1) letter-style%)
(latex-texmacs-section subsubsection (!literal "\\large " 1) letter-style%)
(latex-texmacs-paragraph paragraph 1 letter-style%)
(latex-texmacs-paragraph subparagraph 1 letter-style%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-nullary%
  (labeleqnum "\\addtocounter{equation}{-1}\\refstepcounter{equation}\\addtocounter{equation}{1})")
  (eqnumber (!literal "\\hfill(\\theequation" (!recurse (labeleqnum)) ")"))
  (leqnumber (!literal "(\\theequation" (!recurse (labeleqnum)) ")\\hfill"))
  (reqnumber (!literal "\\hfill(\\theequation" (!recurse (labeleqnum)) ")")))

(drd-table latex-texmacs-unary%
  (key (!literal "\\fbox{\\rule[-2pt]{0pt}{9pt}" (texttt 1) "}"))
  (skey (!recurse (key (!literal "shift-" 1))))
  (ckey (!recurse (key (!literal "ctrl-" 1))))
  (akey (!recurse (key (!literal "alt-" 1))))
  (mkey (!recurse (key (!literal "meta-" 1))))
  (hkey (!recurse (key (!literal "hyper-" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro and environment expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-substitute t args)
  (cond ((number? t) (list-ref args t))
	((== t '---) (car args))
	((func? t '!recurse 1)
	 (latex-expand-macros (latex-substitute (cadr t) args)))
	((func? t '!translate 1)
	 (translate (cadr t) "english" latex-language))
	((list? t) (map (cut latex-substitute <> args) t))
	(else t)))

(tm-define (latex-expand-macros t)
  (if (npair? t) t
      (let* ((head  (car t))
	     (tail  (map latex-expand-macros (cdr t)))
	     (body  (drd-ref latex-texmacs-macro% head latex-style-hyp))
	     (arity (drd-ref latex-texmacs-arity% head latex-style-hyp))
	     (env   (and (func? head '!begin)
			 (drd-ref latex-texmacs-environment% (cadr head))))
	     (envar (and (func? head '!begin)
			 (drd-ref latex-texmacs-env-arity% (cadr head)))))
	(cond ((and body (== (length tail) arity))
	       (latex-substitute body t))
	      ((and env (== (length tail) 1) (== (length (cddr head)) envar))
	       (latex-substitute env (append (cdr t) (cddr head))))
	      (else (cons head tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define latex-macro-table (make-ahash-table))
(define latex-env-table (make-ahash-table))
(define latex-preamble "")

(define (latex-macro-defs-sub t)
  (when (pair? t)
    (for-each latex-macro-defs-sub (cdr t))
    (let* ((body  (drd-ref latex-texmacs-macro% (car t) latex-style-hyp))
	   (arity (drd-ref latex-texmacs-arity% (car t) latex-style-hyp)))
      (when (and body (== (length t) (+ arity 1)))
	(ahash-set! latex-macro-table (car t) (list arity body))
	(latex-macro-defs-sub body)))
    (let* ((body  (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-environment% (cadar t))))
	   (arity (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-env-arity% (cadar t)))))
      (when (and body (== (length (cddar t)) arity))
	(ahash-set! latex-env-table (cadar t) (list arity body))
	(latex-macro-defs-sub body)))
    (with body (or (drd-ref latex-texmacs-preamble% (car t))
		   (and (func? (car t) '!begin)
			(drd-ref latex-texmacs-env-preamble% (cadar t))))
      (when body
	(with s (serialize-latex (latex-expand-def body))
	  (set! latex-preamble (string-append latex-preamble s)))))))

(define (latex-expand-def t)
  (cond ((== t '---) "#-#-#")
	((number? t) (string-append "#" (number->string t)))
	((func? t '!recurse 1) (latex-expand-def (cadr t)))
	((func? t '!translate 1) (translate (cadr t) "english" latex-language))
	((list? t) (map latex-expand-def t))
	(else t)))

(define (latex-macro-def l)
  (with (name arity body) l
    (set! body (serialize-latex (latex-expand-def body)))
    (set! body (string-replace body "\n\n" "*/!!/*"))
    (set! body (string-replace body "\n" " "))
    (set! body (string-replace body "*/!!/*" "\n\n"))
    (string-append "\\newcommand{\\" (symbol->string name) "}"
		   (if (= arity 0) ""
		       (string-append "[" (number->string arity) "]"))
		   "{" body "}\n")))

(define (latex-env-def l)
  (with (name arity body) l
    (set! body (serialize-latex (latex-expand-def body)))
    (set! body (string-replace body "\n\n" "*/!!/*"))
    (set! body (string-replace body "\n  " " "))
    (set! body (string-replace body "\n" " "))
    (set! body (string-replace body "   #-#-# " "}{"))
    (set! body (string-replace body "#-#-# " "}{"))
    (set! body (string-replace body "#-#-#" "}{"))
    (set! body (string-replace body "*/!!/*" "\n\n"))
    (string-append "\\newenvironment{" name "}"
		   (if (= arity 0) ""
		       (string-append "[" (number->string arity) "]"))
		   "{" body "}\n")))

(define (symbol<=? x y)
  (string<=? (symbol->string x) (symbol->string y)))

(tm-define (latex-macro-defs t)
  (set! latex-macro-table (make-ahash-table))
  (set! latex-preamble "")
  (latex-macro-defs-sub t)
  (let* ((l1 (ahash-table->list latex-macro-table))
	 (l2 (list-sort l1 (lambda (x y) (symbol<=? (car x) (car y)))))
	 (l3 (map latex-macro-def l2))
	 (e1 (ahash-table->list latex-env-table))
	 (e2 (list-sort e1 (lambda (x y) (string<=? (car x) (car y)))))
	 (e3 (map latex-env-def e2)))
    (apply string-append (append (list latex-preamble) l3 e3))))
