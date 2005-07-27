
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-drd.scm
;; DESCRIPTION : Formal specification of the part of LaTeX
;;               which is understood by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-drd)
  (:use (convert latex latex-symbol-drd)))

(drd-rules
  ((latex-tag% 'x) (latex-arity% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-command-0%
  #{\ }# #{\;}# #{\,}# #{\:}#
  - / [ ] ! * | i j ss SS oe OE ae AE
  quad qquad par smallskip medskip bigskip
  noindent newline linebreak nolinebreak
  pagebreak nopagebreak newpage newdoublepage clearpage cleardoublepage
  newblock bgroup egroup protect cr date hfill appendix nolimits

  ;; temporarily
  hline

  ;; rewritten
  notin vert Vert addots)

(drd-group latex-command-1%
  usepackage part part* chapter chapter*
  section section* subsection subsection* subsubsection subsubsection*
  paragraph paragraph* subparagraph subparagraph*
  footnote overline underline <sub> <sup> not left right
  big Big bigg Bigg bigl Bigl biggl Biggl
  bigm Bigm biggm Biggm bigr Bigr biggr Biggr
  bar hat tilde widehat widetilde vec grave acute check breve abovering
  dot ddot dddot ddddot
  label ref pageref index hspace hspace* vspace vspace*
  mbox hbox text not
  #{\'}# #{\`}# #{\"}# ^ over ~ = u v H t c d b thispagestyle
  mathord mathbin mathopen mathpunct mathop mathrel mathclose mathalpha
  arabic displaylines cases underbrace overbrace
  includegraphics)

(drd-group latex-command-1% ;; . needs a special treatment
  ,(string->symbol "."))

(drd-group latex-command-2%
  binom choose sideset stackrel citeauthoryear setcounter equal)

(drd-group latex-command-3%
  ifthenelse)

(drd-rules
  ((latex-command% 'x) (latex-command-0% 'x))
  ((latex-arity% 'x 0) (latex-command-0% 'x))
  ((latex-command% 'x) (latex-command-1% 'x))
  ((latex-arity% 'x 1) (latex-command-1% 'x))
  ((latex-command% 'x) (latex-command-2% 'x))
  ((latex-arity% 'x 2) (latex-command-2% 'x))
  ((latex-command% 'x) (latex-command-3% 'x))
  ((latex-arity% 'x 3) (latex-command-3% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands with optional arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-command-0*%
  item #{\\}#)

(drd-group latex-command-1*%
  documentclass documentstyle sqrt bibitem cite)

(drd-group latex-command-2*%
  def newcommand renewcommand frac)

(drd-group latex-command-3*%
  newenvironment renewenvironment)

(drd-rules
  ((latex-command-0% 'x) (latex-command-0*% 'x))
  ((latex-optional-arg% 'x) (latex-command-0*% 'x))
  ((latex-command-1% 'x) (latex-command-1*% 'x))
  ((latex-optional-arg% 'x) (latex-command-1*% 'x))
  ((latex-command-2% 'x) (latex-command-2*% 'x))
  ((latex-optional-arg% 'x) (latex-command-2*% 'x))
  ((latex-command-3% 'x) (latex-command-3*% 'x))
  ((latex-optional-arg% 'x) (latex-command-3*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-environment-0%
  begin-document begin-abstract begin-definition
  begin-theorem begin-proposition begin-lemma begin-corollary
  begin-proof begin-definition begin-notation begin-axiom
  begin-remark begin-warning begin-note
  begin-example begin-exercise begin-verbatim
  begin-matrix begin-pmatrix begin-center
  begin-picture begin-hide-preamble)

(drd-group latex-environment-0*%
  begin-figure begin-table)

(drd-group latex-environment-1%
  begin-proof* begin-tabbing begin-thebibliography)

(drd-group latex-environment-1*%
  begin-array begin-tabular)

(drd-rules
  ((latex-environment% 'x) (latex-environment-0% 'x))
  ((latex-arity% 'x 0) (latex-environment-0% 'x))
  ((latex-environment% 'x) (latex-environment-1% 'x))
  ((latex-arity% 'x 1) (latex-environment-1% 'x))
  ((latex-environment-0% 'x) (latex-environment-0*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-0*% 'x))
  ((latex-environment-1% 'x) (latex-environment-1*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-1*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-modifier-0%
  rm tt sf md bf it em sl sc rmfamily ttfamily sffamily
  mdseries bfseries upshape itshape slshape scshape
  displaystyle textstyle scriptstyle scriptscriptstyle cal frak Bbb
  tiny scriptsize footnotesize small normalsize
  large Large LARGE huge Huge
  black white grey red blue yellow green orange magenta brown pink)

(drd-group latex-modifier-1%
  textrm texttt textsf textmd textbf textup textit textsl textsc emph
  mathrm mathtt mathsf mathmd mathbf mathup mathit mathsl mathnormal
  mathcal mathfrak mathbb mathbbm operatorname boldsymbol)

(drd-rules
  ((latex-modifier% 'x) (latex-modifier-0% 'x))
  ((latex-arity% 'x 0) (latex-modifier-0% 'x))
  ((latex-modifier% 'x) (latex-modifier-1% 'x))
  ((latex-arity% 'x 1) (latex-modifier-1% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special types of LaTeX primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-control%
  $ & % #{\#}# _ { } <less> <gtr>)

(drd-group latex-operator%
  arccos arcsin arctan arg cos cosh cot coth csc deg det dim exp gcd hom
  inf ker lg lim liminf limsup ln log max min Pr sec sin sinh sup tan tanh)

(drd-group latex-list%
  begin-itemize begin-itemizeminus begin-itemizedot begin-itemizearrow
  begin-enumerate begin-enumeratenumeric begin-enumerateroman
  begin-enumerateromancap begin-enumeratealpha begin-enumeratealphacap
  begin-description)

(drd-group latex-math-environment-0%
  begin-formula begin-equation*
  begin-math begin-displaymath begin-equation
  begin-eqnarray begin-eqnarray*
  begin-align begin-align*
  begin-gather begin-gather*
  begin-eqsplit begin-eqsplit*)

(drd-rules
  ((latex-arity% 'x 0) (latex-control% 'x))
  ((latex-arity% 'x 0) (latex-operator% 'x))
  ((latex-environment-0% 'x) (latex-list% 'x))
  ((latex-math-environment% 'x) (latex-math-environment-0% 'x))
  ((latex-environment-0% 'x) (latex-math-environment-0% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-texmacs-0%
  TeXmacs tmbsl tmdummy)

(drd-group latex-texmacs-1%
  tmmathbf tmop tmstrong tmem tmtt tmname
  tmsamp tmabbr tmdfn tmkbd tmvar tmacronym tmperson tmscript)

(drd-group latex-texmacs-2%
  tmhlink tmaction)

(drd-rules
  ((latex-texmacs% 'x) (latex-texmacs-0% 'x))
  ((latex-arity% 'x 0) (latex-texmacs-0% 'x))
  ((latex-texmacs% 'x) (latex-texmacs-1% 'x))
  ((latex-arity% 'x 1) (latex-texmacs-1% 'x))
  ((latex-texmacs% 'x) (latex-texmacs-2% 'x))
  ((latex-arity% 'x 2) (latex-texmacs-2% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-symbol%
  ;; Greek letters
  Gamma Delta Theta Lambda Xi Pi Sigma Upsilon Phi Psi Omega
  alpha beta gamma delta epsilon
  varepsilon zeta eta theta vartheta
  iota kappa lambda mu nu omicron
  xi pi varpi rho
  varrho sigma varsigma tau upsilon
  phi varphi chi psi omega

  ;; Binary operations
  pm mp times div ast star circ bullet cdot
  cap cup uplus sqcap sqcup vee wedge setminus wr
  diamond triangleleft triangleright
  oplus ominus otimes oslash odot bigcirc amalg

  ;; Relations
  leq le geq ge equiv models prec
  succ sim perp preceq succeq
  simeq mid ll gg asymp
  parallel subset supset approx bowtie
  subseteq supseteq cong Join sqsubset
  sqsupset ne neq smile sqsubseteq sqsupseteq
  doteq frown in ni propto
  vdash dashv
  
  ;; Arrows
  leftarrow rightarrow uparrow downarrow
  Leftarrow Rightarrow Uparrow Downarrow
  nearrow searrow swarrow nwarrow
  leftrightarrow updownarrow Updownarrow Leftrightarrow 
  leftharpoonup leftharpoondown rightharpoonup rightharpoondown
  hookleftarrow hookrightarrow
  to mapsto longmapsto
  longrightarrow longleftarrow longleftrightarrow
  Longrightarrow Longleftarrow Longleftrightarrow 
  
  ;; Miscellaneous symbols
  ldots cdots vdots ddots aleph
  prime forall infty hbar emptyset
  exists nabla surd triangle
  imath jmath ell neg
  top flat natural sharp wp
  bot clubsuit diamondsuit heartsuit spadesuit
  Re Im angle partial
  dag ddag dagger ddagger

  ;; Delimiters
  uparrow Uparrow downarrow Downarrow
  updownarrow Updownarrow
  lfloor rfloor lceil rceil
  langle rangle backslash

  ;; Big delimiters
  rmoustache lmoustache rgroup lgroup
  arrowvert Arrowvert bracevert

  ;; Binary operations (latexsym or amssymb required)
  lhd rhd unlhd unrhd

  ;; Miscellaneous symbols (amssymb or latexsym required)
  Box Diamond mho)

(drd-group latex-texmacs-symbol%
  ;; TeXmacs specific symbols
  mathd mathe mathi mathpi
  nin udots um assign plusassign minusassign
  trianglelefteqslant trianglerighteqslant
  leftarrowlim rightarrowlim leftrightarrowlim mapstolim
  equallim Leftarrowlim Rightarrowlim Leftrightarrowlim
  longleftarrowlim longrightarrowlim leftrightarrowlim longmapstolim
  longequallim Longleftarrowlim Longrightarrowlim Longleftrightarrowlim)

(drd-group latex-big-symbol%
  sum int bigintwl oint bigointwl prod coprod
  bignone bigtimes bigoplus bigotimes bigodot
  bigvee bigwedge bigsqcup bigcup bigcap bigpluscup bigtriangledown
  bigtriangleup bigcurlyvee bigcurlywedge bigsqcap bigbox bigparallel
  biginterleave bignplus bigvarint bigiint bigiiint bigvaroint bigoiint)

(drd-rules
  ((latex-arity% 'x 0) (latex-symbol% 'x))
  ((latex-arity% 'x 0) (latex-big-symbol% 'x))
  ((latex-symbol% 'x) (latex-ams-symbol% 'x))
  ((latex-symbol% 'x) (latex-wasy-symbol% 'x))
  ((latex-symbol% 'x) (latex-stmary-symbol% 'x))
  ((latex-symbol% 'x) (latex-texmacs-symbol% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-package-priority%
  ("amsmath" 0)
  ("amssymb" 10)
  ("graphicx" 20)
  ("wasysym" 30)
  ("stmaryd" 40)
  ("enumerate" 50)
  ("epsfig" 60)
  ("mathrsfs" 70)
  ("bbm" 80)
  ("dsfont" 90)
  ("euscript" 100)
  ("multicol" 110))

(drd-table latex-needs%
  (mathscr "mathrsfs")
  (EuScript "euscript")
  (mathbbm "bbm")
  (mathbbmss "bbm")
  (mathds "dsfont")
  (mathfrak "amssymb")
  (mathbb "amssymb")
  
  (proof "amssymb")
  (proof* "amssymb")

  (epsfig "epsfig")

  (text "amsmath")
  (dddot "amsmath")
  (ddddot "amsmath")
  (overset "amsmath")
  (underset "amsmath")
  (tmop "amsmath")
  (tmmathbf "amsmath")
  (lleq "amsmath")
  (llleq "amsmath")
  (ggeq "amsmath")
  (gggeq "amsmath")

  (enumeratealpha "enumerate")
  (enumeratealphacap "enumerate")
  (enumeratenumeric "enumerate")
  (enumerateroman "enumerate")
  (enumerateromancap "enumerate")

  (Backepsilon "graphicx")
  (Mho "graphicx")
  (mho "graphicx")
  (udots "graphicx")

  (tmfloat "ifthen")
  (tmfloat "capt-of")
  (tmfloat "calc")
  
  (color "color")
  (tmhlink "color")
  (tmaction "color")

  (omicron "pslatex")
  (multicols "multicol")
  (bundle "epic")
  (chunk "epic")
  (bundle "ecltree")
  (chunk "ecltree"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated routines for consulting the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-resolve s)
  (if (string-starts? s "\\")
      (set! s (substring s 1 (string-length s))))
  (with arity (drd-ref latex-arity% (string->symbol s))
    (if (drd-in? (string->symbol s) latex-optional-arg%)
	(set! arity (- -1 arity)))
    (if (string-starts? s "end-")
	(begin
	  (set! s (string-append "begin-" (substring s 4 (string-length s))))
	  (set! arity 0)))
    (if (string-ends? s "*")
	(begin
	  (if (!= s "*")
	      (set! s (substring s 0 (- (string-length s) 1))))
	  (if (not arity)
	      (set! arity (drd-ref latex-arity% (string->symbol s))))))
    (values (string->symbol s) arity)))

(tm-define (latex-arity tag)
  "Get the arity of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (if arity arity 0)))

(tm-define (latex-type tag)
  "Get the type of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (cond ((not arity) "undefined")
          ((drd-in? s latex-command%) "command")
	  ((drd-in? s latex-modifier%) "modifier")
	  ((drd-in? s latex-control%) "control")
	  ((drd-in? s latex-operator%) "operator")
	  ((drd-in? s latex-list%) "list")
	  ((drd-in? s latex-math-environment%) "math-environment")
	  ((drd-in? s latex-environment%) "environment")
	  ((drd-in? s latex-texmacs%) "texmacs")
	  ((drd-in? s latex-symbol%) "symbol")
	  ((drd-in? s latex-big-symbol%) "big-symbol")
	  (else "undefined"))))
