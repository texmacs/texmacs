
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-texmacs-drd.scm
;; DESCRIPTION : TeXmacs extensions to LaTeX
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-texmacs-drd))

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
  (asympasymp "{\\asymp\\!\\!\\!\\!\\!\\!-}")
  (nasympasymp "{\\not\\asymp\\!\\!\\!\\!\\!\\!-}")
  (simsim "{\\approx\\!\\!\\!\\!\\!\\!-}")
  (nsimsim "{\\not\\approx\\!\\!\\!\\!\\!\\!-}")
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
  (btimes "{\\mbox{\\rotatebox[origin=c]{90}{$\\ltimes$}}}")
  (Backepsilon "{\\mbox{\\rotatebox[origin=c]{180}{E}}}")
  (Mho "{\\mbox{\\rotatebox[origin=c]{180}{$\\Omega$}}}")
  (mathcatalan "C")
  (mathd "\\mathrm{d}")
  (mathe "\\mathrm{e}")
  (matheuler "\\gamma")
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
  (Exists "\\exists")
  (bigintwl "\\int")
  (bigointwl "\\oint")
  (bignone "")
  (asterisk "*")
  (nonesep "")
  (nin "\\not\\in")
  (nni "\\not\\ni")
  (notni "\\not\\ni")
  (nequiv "\\not\\equiv")
  (dotamalg "\\mathaccent95{\\amalg}")
  (dottimes "\\mathaccent95{\\times}")
  (dotoplus "\\mathaccent95{\\oplus}")
  (dototimes "\\mathaccent95{\\otimes}")
  (into "\\rightarrow")
  (longequal "{=\\!\\!=}")
  (longhookrightarrow "{\\lhook\\joinrel\\relbar\\joinrel\\rightarrow}")
  (longhookleftarrow "{\\leftarrow\\joinrel\\relbar\\joinrel\\rhook}")
  (longdownarrow "\\downarrow")
  (longuparrow "\\uparrow")
  (triangleup "\\triangle")
  (precdot "{\\prec\\hspace{-0.6em}\\cdot}\\;\\,")
  (preceqdot "{\\preccurlyeq\\hspace{-0.6em}\\cdot}\\;\\,")
  (llangle "{\\langle\\!\\langle}")
  (rrangle "{\\rangle\\!\\rangle}")
  (join "\\Join")
  (um "-")
  (upl "+")
  (upm "\\pm")
  (ump "\\mp")
  (upequal "{\\mbox{\\rotatebox[origin=c]{90}{$=$}}}")
  (assign ":=")
  (plusassign "+\\!\\!=")
  (minusassign "-\\!\\!=")
  (timesassign "\times\\!\\!=")
  (overassign "/\\!\\!=")
  (lflux "\\ll")
  (gflux "\\gg")
  (colons "\\,:\\,")
  (transtype "\\,:\\!!>")
  (udots "{\\mathinner{\\mskip1mu\\raise1pt\\vbox{\\kern7pt\\hbox{.}}\\mskip2mu\\raise4pt\\hbox{.}\\mskip2mu\\raise7pt\\hbox{.}\\mskip1mu}}"))

(drd-rules
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-symbol% 'x 'body))
  ((latex-texmacs-arity% 'x 0) (latex-texmacs-symbol% 'x 'body))
  ;;;
  ((latex-texmacs-tag% 'x) (latex-texmacs-macro% 'x 'body))
  ((latex-arity% 'x 'arity) (latex-texmacs-arity% 'x 'arity))
  ((latex-symbol% 'x) (latex-texmacs-symbol% 'x 'body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-0%
  (tmunsc "\\_")
  (emdash "---")
  (tmat "\\symbol{\"40}")
  (tmbsl "\\ensuremath{\\backslash}")
  (tmdummy "$\\mbox{}$")
  (TeXmacs "T\\kern-.1667em\\lower.5ex\\hbox{E}\\kern-.125emX\\kern-.1em\\lower.5ex\\hbox{\\textsc{m\\kern-.05ema\\kern-.125emc\\kern-.05ems}}")
  (madebyTeXmacs (footnote (!recurse (withTeXmacstext))))
  (withTeXmacstext
   (!append (!translate "This document has been produced using") " "
	    (!recurse (TeXmacs)) " (" (!translate "see") " "
	    (texttt "http://www.texmacs.org") ")"))
  (scheme "{\\sc Scheme}")
  (pari "{\\sc Pari}"))

(drd-table latex-texmacs-1%
  (tmtextrm (!group (rmfamily) (!group 1)))
  (tmtextsf (!group (sffamily) (!group 1)))
  (tmtexttt (!group (ttfamily) (!group 1)))
  (tmtextmd (!group (mdseries) (!group 1)))
  (tmtextbf (!group (bfseries) (!group 1)))
  (tmtextup (!group (upshape) (!group 1)))
  (tmtextsl (!group (slshape) (!group 1)))
  (tmtextit (!group (itshape) (!group 1)))
  (tmtextsc (!group (scshape) (!group 1)))
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
  (dueto (textup (textbf (!append "(" 1 ") "))))
  (op 1)
  (email (!group (textit (!translate "Email:")) " " (texttt 1)))
  (homepage (!group (textit (!translate "Web:")) " "(texttt 1)))
  (keywords (!group (textbf (!translate "Keywords:")) " " 1))
  (AMSclass (!group (textbf (!translate "A.M.S. subject classification:"))
		    " " 1)))

(drd-table latex-texmacs-2%
  (tmhlink (!group "\\color{blue} " 1))
  (tmaction (!group "\\color{blue} " 1))
  (subindex (index (!append 1 "!" 2))))

(drd-table latex-texmacs-3%
  (subsubindex (index (!append 1 "!" 2 "!" 3)))
  (tmref 1)
  (glossaryentry (!append (item (!option (!append 1 (hfill)))) 2 (dotfill) 3)))

(drd-table latex-texmacs-4%
  (subsubsubindex (index (!append 1 "!" 2 "!" 3 "!" 4))))

(drd-rules
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-0% 'x 'body))
  ((latex-texmacs-arity% 'x 0) (latex-texmacs-0% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-1% 'x 'body))
  ((latex-texmacs-arity% 'x 1) (latex-texmacs-1% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-2% 'x 'body))
  ((latex-texmacs-arity% 'x 2) (latex-texmacs-2% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-3% 'x 'body))
  ((latex-texmacs-arity% 'x 3) (latex-texmacs-3% 'x 'body))
  ((latex-texmacs-macro% 'x 'body) (latex-texmacs-4% 'x 'body))
  ((latex-texmacs-arity% 'x 4) (latex-texmacs-4% 'x 'body))
  ;;;
  ((latex-texmacs% 'x) (latex-texmacs-0% 'x 'body))
  ((latex-texmacs% 'x) (latex-texmacs-1% 'x 'body))
  ((latex-texmacs% 'x) (latex-texmacs-2% 'x 'body))
  ((latex-texmacs% 'x) (latex-texmacs-3% 'x 'body))
  ((latex-texmacs% 'x) (latex-texmacs-4% 'x 'body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-environment%
  ("proof"
   (!append (noindent) (textbf (!append (!translate "Proof") "\\ "))
	    ---
	    (hspace* (fill)) (!math (Box)) (medskip)))
  ("proof*"
   (!append (noindent) (textbf (!append 1 "\\ "))
	    ---
	    (hspace* (fill)) (!math (Box)) (medskip)))
  ("tmparmod"
   ((!begin "list" "" (!append "\\setlength{\\topsep}{0pt}"
			       "\\setlength{\\leftmargin}{" 1 "}"
			       "\\setlength{\\rightmargin}{" 2 "}"
			       "\\setlength{\\parindent}{" 3 "}"
			       "\\setlength{\\listparindent}{\\parindent}"
			       "\\setlength{\\itemindent}{\\parindent}"
			       "\\setlength{\\parsep}{\\parskip}"))
    (!append "\\item[]"
	     ---)))
  ("tmparsep"
   (!append (begingroup) "\\setlength{\\parskip}{" 1 "}"
	     ---
	     (endgroup)))
  ("tmindent"
   ((!begin "tmparmod" "1.5em" "0pt" "0pt") ---))
  ("elsequation" ((!begin "eqnarray") (!append --- "&&")))
  ("elsequation*" ((!begin "eqnarray*") (!append --- "&&")))
  ("theglossary"
   ((!begin "list" "" (!append "\\setlength{\\labelwidth}{6.5em}"
			       "\\setlength{\\leftmargin}{7em}"
			       "\\small")) ---)))

(drd-table latex-texmacs-env-arity%
  ("proof" 0)
  ("proof*" 1)
  ("tmparmod" 3)
  ("tmparsep" 1)
  ("tmindent" 0)
  ("proof" 0)
  ("elsequation" 0)
  ("elsequation*" 0)
  ("theglossary" 1))

(drd-group latex-texmacs-tag%
  begin-proof begin-proof* begin-tmparmod begin-tmparsep begin-tmindent
  begin-elsequation begin-elsequation* begin-theglossary)

(drd-group latex-environment-0%
  begin-proof begin-tmindent begin-elsequation begin-elsequation*)

(drd-group latex-environment-1%
  begin-proof* begin-theglossary)

(drd-group latex-environment-3%
  begin-tmparmod begin-tmparsep)

(define-macro (latex-texmacs-itemize env lab)
  (with env-sym (string->symbol (string-append "begin-" env))
    `(begin
       (drd-table latex-texmacs-environment%
	 (,env
	  ((!begin "itemize")
	   (!append "\\renewcommand{\\labelitemi}{" ,lab "}"
		    "\\renewcommand{\\labelitemii}{" ,lab "}"
		    "\\renewcommand{\\labelitemiii}{" ,lab "}"
		    "\\renewcommand{\\labelitemiv}{" ,lab "}"
		    ---))))
       (drd-table latex-texmacs-env-arity% (,env 0))
       ;;;
       (drd-group latex-texmacs-tag% ,env-sym)
       (drd-group latex-list% ,env-sym))))

(define-macro (latex-texmacs-enumerate env lab)
  (with env-sym (string->symbol (string-append "begin-" env))
    `(begin
       (drd-table latex-texmacs-environment%
	 (,env ((!begin "enumerate" (!option ,lab)) ---)))
       (drd-table latex-texmacs-env-arity% (,env 0))
       ;;;
       (drd-group latex-texmacs-tag% ,env-sym)
       (drd-group latex-list% ,env-sym))))

(define-macro (latex-texmacs-description env)
  (with env-sym (string->symbol (string-append "begin-" env))
    `(begin
       (drd-table latex-texmacs-environment%
	 (,env ((!begin "description") ---)))
       (drd-table latex-texmacs-env-arity% (,env 0))
       ;;;
       (drd-group latex-texmacs-tag% ,env-sym)
       (drd-group latex-list% ,env-sym))))

(latex-texmacs-itemize "itemizeminus" "$-$")
(latex-texmacs-itemize "itemizedot" "$\\bullet$")
(latex-texmacs-itemize "itemizearrow" "$\\rightarrow$")
(latex-texmacs-enumerate "enumeratenumeric" "1.")
(latex-texmacs-enumerate "enumerateroman" "i.")
(latex-texmacs-enumerate "enumerateromancap" "I.")
(latex-texmacs-enumerate "enumeratealpha" "a{\\textup{)}}")
(latex-texmacs-enumerate "enumeratealphacap" "A.")
(latex-texmacs-description "descriptioncompact")
(latex-texmacs-description "descriptionaligned")
(latex-texmacs-description "descriptiondash")
(latex-texmacs-description "descriptionlong")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra preamble definitions which are needed to export certain macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-preamble%
  (mho
   (!append
    "\\renewcommand{\\mho}{\\mbox{\\rotatebox[origin=c]{180}{$\\omega$}}}"))
  (color
   (!append
    "\\definecolor{grey}{rgb}{0.75,0.75,0.75}\n"
    "\\definecolor{orange}{rgb}{1.0,0.5,0.5}\n"
    "\\definecolor{brown}{rgb}{0.5,0.25,0.0}\n"
    "\\definecolor{pink}{rgb}{1.0,0.5,0.5}\n"))
  (tmfloat
   (!append
    "\\newcommand{\\tmfloatcontents}{}\n"
    "\\newlength{\\tmfloatwidth}\n"
    "\\newcommand{\\tmfloat}[5]{\n"
    "  \\renewcommand{\\tmfloatcontents}{#4}\n"
    "  \\setlength{\\tmfloatwidth}{\\widthof{\\tmfloatcontents}+1in}\n"
    "  \\ifthenelse{\\equal{#2}{small}}\n"
    "    {\\ifthenelse{\\lengthtest{\\tmfloatwidth > \\linewidth}}\n"
    "      {\\setlength{\\tmfloatwidth}{\\linewidth}}{}}\n"
    "    {\\setlength{\\tmfloatwidth}{\\linewidth}}\n"
    "  \\begin{minipage}[#1]{\\tmfloatwidth}\n"
    "    \\begin{center}\n"
    "      \\tmfloatcontents\n"
    "      \\captionof{#3}{#5}\n"
    "    \\end{center}\n"
    "  \\end{minipage}}\n")))

(define-macro (latex-texmacs-thmenv prim name before after)
  (with env-sym (string->symbol (string-append "begin-" prim))
    `(begin
       (drd-table latex-texmacs-env-preamble%
	 (,prim (!append ,@before (newtheorem ,prim (!translate ,name))
			 ,@after "\n")))
       ;;;
       (drd-group latex-texmacs-tag% ,env-sym)
       (drd-group latex-environment-0% ,env-sym))))

(define-macro (latex-texmacs-theorem prim name)
  `(latex-texmacs-thmenv ,prim ,name () ()))

(define-macro (latex-texmacs-remark prim name)
  `(latex-texmacs-thmenv
    ,prim ,name ("{" (!recurse (theorembodyfont "\\rmfamily"))) ("}")))

(define-macro (latex-texmacs-exercise prim name)
  `(latex-texmacs-thmenv
    ,prim ,name ("{" (!recurse (theorembodyfont "\\rmfamily\\small"))) ("}")))

(latex-texmacs-theorem "theorem" "Theorem")
(latex-texmacs-theorem "proposition" "Proposition")
(latex-texmacs-theorem "lemma" "Lemma")
(latex-texmacs-theorem "corollary" "Corollary")
(latex-texmacs-theorem "axiom" "Axiom")
(latex-texmacs-theorem "definition" "Definition")
(latex-texmacs-theorem "notation" "Notation")
(latex-texmacs-theorem "conjecture" "Conjecture")
(latex-texmacs-remark "remark" "Remark")
(latex-texmacs-remark "note" "Note")
(latex-texmacs-remark "example" "Example")
(latex-texmacs-remark "convention" "Convention")
(latex-texmacs-remark "warning" "Warning")
(latex-texmacs-exercise "exercise" "Exercise")
(latex-texmacs-exercise "problem" "Problem")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style-dependent extra macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-0%
  (appendix "" letter-style%))

(define-macro (latex-texmacs-section name inside . conds)
  `(drd-table latex-texmacs-1%
     (,name (!append (medskip) (bigskip) "\n\n" (noindent) (textbf ,inside))
	    ,@conds)))

(define-macro (latex-texmacs-paragraph name inside . conds)
  `(drd-table latex-texmacs-1%
     (,name (!append (smallskip) "\n\n" (noindent) (textbf ,inside))
	    ,@conds)))

(latex-texmacs-section chapter (!append "\\huge " 1) article-style%)
(latex-texmacs-section chapter (!append "\\huge " 1) letter-style%)
(latex-texmacs-section section (!append "\\LARGE " 1) letter-style%)
(latex-texmacs-section subsection (!append "\\Large " 1) letter-style%)
(latex-texmacs-section subsubsection (!append "\\large " 1) letter-style%)
(latex-texmacs-paragraph paragraph 1 letter-style%)
(latex-texmacs-paragraph subparagraph 1 letter-style%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated extra macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-0%
  (labeleqnum "\\addtocounter{equation}{-1}\\refstepcounter{equation}\\addtocounter{equation}{1})")
  (eqnumber (!append "\\hfill(\\theequation" (!recurse (labeleqnum)) ")"))
  (leqnumber (!append "(\\theequation" (!recurse (labeleqnum)) ")\\hfill"))
  (reqnumber (!append "\\hfill(\\theequation" (!recurse (labeleqnum)) ")")))

(drd-table latex-texmacs-1%
  (key (!append "\\fbox{\\rule[-2pt]{0pt}{9pt}" (texttt 1) "}"))
  (skey (!recurse (key (!append "shift-" 1))))
  (ckey (!recurse (key (!append "ctrl-" 1))))
  (akey (!recurse (key (!append "alt-" 1))))
  (mkey (!recurse (key (!append "meta-" 1))))
  (hkey (!recurse (key (!append "hyper-" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language specific preambles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-preamble-language-def%
  ("bulgarian"
   "\\usepackage[cp1251]{inputenc}\n\\usepackage[bulgarian]{babel}")
  ("czech" "\\usepackage[czech]{babel}")
  ("danish" "\\usepackage[danish]{babel}")
  ("dutch" "\\usepackage[dutch]{babel}")
  ("finnish" "\\usepackage[finnish]{babel}")
  ("french" "\\usepackage[french]{babel}")
  ("german" "\\usepackage[german]{babel}")
  ("hungarian" "\\usepackage[hungarian]{babel}")
  ("italian" "\\usepackage[italian]{babel}")
  ("polish" "\\usepackage[polish]{babel}")
  ("portuguese" "\\usepackage[portuges]{babel}")
  ("romanian" "\\usepackage[romanian]{babel}")
  ("russian" "\\usepackage[cp1251]{inputenc}\n\\usepackage[russian]{babel}")
  ("slovene" "\\usepackage[slovene]{babel}")
  ("spanish" "\\usepackage[spanish]{babel}")
  ("swedish" "\\usepackage[swedish]{babel}")
  ("ukrainian"
   "\\usepackage[cp1251]{inputenc}\n\\usepackage[ukrainian]{babel}"))

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
  ("Ã" "\\~A")
  ("ã" "\\~a")
  ("Æ" "{\\AE}")
  ("æ" "{\\ae}")
  ("ç" "\\c{c}")
  ("Ç" "\\c{C}")
  ("ð" "{\\dh}")
  ("Ð" "{\\DH}")
  ("è" "\\`e")
  ("È" "\\`E")
  ("é" "\\'e")
  ("É" "\\'E")
  ("ë" "\\\"e")
  ("Ë" "\\\"E")
  ("ê" "\\^e")
  ("Ê" "\\^E")
  ("ì" "\\`{\\i}")
  ("Ì" "\\`I")
  ("í" "\\'{\\i}")
  ("Í" "\\'I")
  ("ï" "\\\"{\\i}")
  ("Ï" "\\\"I")
  ("î" "\\^{\\i}")
  ("Î" "\\^I")
  ("Ñ" "\\~N")
  ("ñ" "\\~n")
  ("ò" "\\`o")
  ("Ò" "\\`O")
  ("ó" "\\'o")
  ("Ó" "\\'O")
  ("ö" "\\\"o")
  ("Ö" "\\\"O")
  ("ô" "\\^o")
  ("Ô" "\\^O")
  ("Õ" "\\~O")
  ("õ" "\\~o")
  ("Ø" "{\\O}")
  ("ø" "{\\o}")
  ("ß" "{\\ss}")
  ("þ" "{\\th}")
  ("Þ" "{\\TH}")
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
  ("¾" "\\\"Y")
  ("¡" "!`")
  ("" "?`")
  ("¿" "?`"))

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
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-paper-type%
  ("a0" "a0paper")
  ("a1" "a1paper")
  ("a2" "a2paper")
  ("a3" "a3paper")
  ("a4" "a4paper")
  ("a5" "a5paper")
  ("a6" "a6paper")
  ("a7" "papersize={74mm,105mm}")
  ("a8" "papersize={52mm,74mm")
  ("a9" "papersize={37mm,52mm}")
  ("b0" "b0paper")
  ("b1" "b1paper")
  ("b2" "b2paper")
  ("b3" "b3paper")
  ("b4" "b4paper")
  ("b5" "b5paper")
  ("b6" "b6paper")
  ("b7" "papersize={88mm,125mm}")
  ("b8" "papersize={62mm,88mm}")
  ("b9" "papersize={44mm,62mm}")
  ("legal" "legalpaper")
  ("letter" "letterpaper")
  ("executive" "executivepaper")
  ("archA" "papersize={9in,12in}")
  ("archB" "papersize={12in,18in}")
  ("archC" "papersize={18in,24in}")
  ("archD" "papersize={24in,36in}")
  ("archE" "papersize={36in,48in}")
  ("10x14" "papersize={10in,14in}")
  ("11x17" "papersize={11in,17in}")
  ("C5" "papersize={162mm,229mm}")
  ("Comm10" "papersize={297pt,684pt}")
  ("DL" "papersize={110mm,220mm}")
  ("halfletter" "papersize={140mm,216mm}")
  ("halfexecutive" "papersize={133mm,184mm}")
  ("ledger" "papersize={432mm,279mm}")
  ("Monarch" "papersize={98mm,190mm}")
  ("csheet" "papersize={432mm,559mm}")
  ("dsheet" "papersize={559mm,864mm}")
  ("esheet" "papersize={864mm,1118mm}")
  ("flsa" "papersize={216mm,330mm}")
  ("flse" "papersize={216mm,330mm}")
  ("folio" "papersize={216mm,330mm}")
  ("lecture note" "papersize={15.5cm,23.5cm}")
  ("note" "papersize={216mm,279mm}")
  ("quarto" "papersize={215mm,275mm}")
  ("statement" "papersize={140mm,216mm}")
  ("tabloid" "papersize={279mm,432mm}"))
