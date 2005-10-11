
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-extend-drd.scm
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

(texmacs-module (convert latex latex-extend-drd))

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

(tm-define (latex-set-language lan)
  (set! latex-language lan)
  (set! latex-cyrillic-catcode?
	(in? lan '("bulgarian" "russian" "ukrainian"))))

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
;; Extra symbols defined by TeXmacs
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
;; Macro expansion and definitions
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
   (!concat* (!translate "This document has been produced using") " "
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
  (dueto (textup (textbf (!concat* "(" 1 ") "))))
  (op 1)
  (email (!group (textit (!translate "Email:")) " " (texttt 1)))
  (homepage (!group (textit (!translate "Web:")) " "(texttt 1)))
  (keywords (!group (textbf (!translate "Keywords:")) " " 1))
  (AMSclass (!group (textbf (!translate "A.M.S. subject classification:"))
		    " " 1)))

(drd-table latex-texmacs-binary%
  (tmhlink (!group "\\color{blue} " 1))
  (tmaction (!group "\\color{blue} " 1))
  (subindex (index (!concat* 1 "!" 2))))

(drd-table latex-texmacs-ternary%
  (subsubindex (index (!concat* 1 "!" 2 "!" 3)))
  (tmref 1))

(drd-table latex-texmacs-tetrary%
  (subsubindex (index (!concat* 1 "!" 2 "!" 3 "!" 4))))

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
;; Deprecated macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-texmacs-nullary%
  (labeleqnum "\\addtocounter{equation}{-1}\\refstepcounter{equation}\\addtocounter{equation}{1})")
  (eqnumber (!concat* "\\hfill(\\theequation" (!recurse (labeleqnum)) ")"))
  (leqnumber (!concat* "(\\theequation" (!recurse (labeleqnum)) ")\\hfill"))
  (reqnumber (!concat* "\\hfill(\\theequation" (!recurse (labeleqnum)) ")")))

(drd-table latex-texmacs-unary%
  (key (!concat* "\\fbox{\\rule[-2pt]{0pt}{9pt}" (texttt 1) "}"))
  (skey (!recurse (key (!concat* "shift-" 1))))
  (ckey (!recurse (key (!concat* "ctrl-" 1))))
  (akey (!recurse (key (!concat* "alt-" 1))))
  (mkey (!recurse (key (!concat* "meta-" 1))))
  (hkey (!recurse (key (!concat* "hyper-" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro expansion and definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-substitute t args)
  (cond ((number? t) (list-ref args t))
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
	     (body  (drd-ref latex-texmacs-macro% head))
	     (arity (drd-ref latex-texmacs-arity% head)))
	(if (and body (== (length tail) arity))
	    (latex-substitute body t)
	    (cons head tail)))))

(define latex-macro-table (make-ahash-table))

(define (latex-macro-defs-sub t)
  (when (pair? t)
    (for-each latex-macro-defs-sub (cdr t))
    (let* ((body  (drd-ref latex-texmacs-macro% (car t)))
	   (arity (drd-ref latex-texmacs-arity% (car t))))
      (when (and body (== (length t) (+ arity 1)))
	(ahash-set! latex-macro-table (car t) (list arity body))
	(latex-macro-defs-sub body)))))

(define (latex-expand-def t)
  (cond ((number? t) (string-append "#" (number->string t)))
	((func? t '!recurse 1) (latex-expand-def (cadr t)))
	((func? t '!translate 1) (translate (cadr t) "english" latex-language))
	((list? t) (map latex-expand-def t))
	(else t)))

(define (latex-macro-def l)
  (with (name arity body) l
    (display* name ", " arity ", " body "\n")
    (set! body (serialize-latex (latex-expand-def body)))
    (set! body (string-replace body "\n" " "))
    (string-append "\\newcommand{\\" (symbol->string name) "}"
		   (if (= arity 0) ""
		       (string-append "[" (number->string arity) "]"))
		   "{" body "}\n")))

(define (symbol<=? x y)
  (string<=? (symbol->string x) (symbol->string y)))

(tm-define (latex-macro-defs t)
  (set! latex-macro-table (make-ahash-table))
  (latex-macro-defs-sub t)
  (let* ((l1 (ahash-table->list latex-macro-table))
	 (l2 (list-sort l1 (lambda (x y) (symbol<=? (car x) (car y)))))
	 (l3 (map latex-macro-def l2)))
    (apply string-append l3)))
