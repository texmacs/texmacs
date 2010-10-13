
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-drd.scm
;; DESCRIPTION : Formal specification of the part of LaTeX
;;               which is understood by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-drd)
  (:use (convert latex latex-symbol-drd)
	(convert latex latex-texmacs-drd)))

(drd-rules
  ((latex-tag% 'x) (latex-arity% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-group latex-command-0%
  ,(string->symbol " ") ,(string->symbol ";") 
  ,(string->symbol ",") ,(string->symbol ":") 
  - / [ ] ! * | i j ss SS oe OE ae AE
  AA DH L NG O S TH aa dh dj l ng o th pounds
  quad qquad par smallskip medskip bigskip
  noindent newline linebreak nolinebreak
  pagebreak nopagebreak newpage newdoublepage clearpage cleardoublepage
  newblock bgroup egroup protect cr date hfill appendix nolimits dots
  maketitle tableofcontents TeX LaTeX
  begingroup endgroup

  ;; temporarily
  hline
  ;; rewritten
  notin vert Vert addots
  ;; wikipedia
  infin rang)

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
  ,(string->symbol "'") ,(string->symbol "`") ,(string->symbol "\"")
  ^ over atop ~ = u v H t c d b k r thispagestyle ensuremath
  mathord mathbin mathopen mathpunct mathop mathrel mathclose mathalpha
  arabic displaylines cases underbrace overbrace
  title author thanks
  phantom hphantom vphantom smash
  newcounter stepcounter refstepcounter value
  citet citep citet* citep* citealt citealp citealt* citealp*
  citetext citeauthor citeauthor* citeyear
  includegraphics url penalty
  enlargethispage)

(drd-group latex-command-1% ;; . needs a special treatment
  ,(string->symbol "."))

(drd-group latex-command-2%
  binom choose cfrac tfrac
  sideset stackrel citeauthoryear
  equal href
  setcounter addtocounter setlength addtolength)

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
  item ,(string->symbol "\\"))

(drd-group latex-command-1*%
  documentclass documentstyle sqrt bibitem cite)

(drd-group latex-command-2*%
  def newcommand renewcommand newtheorem frac)

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
  begin-document begin-abstract begin-verbatim
  begin-matrix begin-pmatrix begin-bmatrix begin-vmatrix
  begin-center begin-picture)

(drd-group latex-environment-0*%
  begin-figure begin-table)

(drd-group latex-environment-1%
  begin-tabbing begin-thebibliography)

(drd-group latex-environment-1*%
  begin-array begin-tabular)

(drd-rules
  ((latex-environment% 'x) (latex-environment-0% 'x))
  ((latex-arity% 'x 0) (latex-environment-0% 'x))
  ((latex-environment% 'x) (latex-environment-1% 'x))
  ((latex-arity% 'x 1) (latex-environment-1% 'x))
  ((latex-environment% 'x) (latex-environment-2% 'x))
  ((latex-arity% 'x 2) (latex-environment-2% 'x))
  ((latex-environment% 'x) (latex-environment-3% 'x))
  ((latex-arity% 'x 3) (latex-environment-3% 'x))
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
  $ & % ,(string->symbol "#") _ { } <less> <gtr>)

(drd-group latex-operator%
  arccos arcsin arctan arg cos cosh cot coth csc deg det dim exp gcd hom
  inf ker lg lim liminf limsup ln log max min Pr sec sin sinh sup tan tanh)

(drd-group latex-list%
  begin-itemize begin-enumerate begin-description)

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
  oplus ominus otimes oslash odot bigcirc amalg notin

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

  ;; Miscellaneous symbols (amssymb or graphicx required)
  Diamond mho)

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
  ((latex-symbol% 'x) (latex-stmary-symbol% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table latex-package-priority%
  ("geometry" 10)
  ("amsmath" 20)
  ("amssymb" 30)
  ("graphicx" 40)
  ("wasysym" 50)
  ("stmaryrd" 60)
  ("enumerate" 70)
  ("epsfig" 80)
  ("mathrsfs" 90)
  ("bbm" 100)
  ("dsfont" 110)
  ("euscript" 120)
  ("multicol" 130)
  ("hyperref" 140))

(drd-table latex-needs%
  (geometry "geometry")
  (epsfig "epsfig")
  (includegraphics "graphicx")

  (mathscr "mathrsfs")
  (EuScript "euscript")
  (mathbbm "bbm")
  (mathbbmss "bbm")
  (mathds "dsfont")
  (mathfrak "amssymb")
  (mathbb "amssymb")
  (theorembodyfont "theorem")

  (Diamond "amssymb")
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
  (btimes "graphicx")
  (Backepsilon "graphicx")
  (Mho "graphicx")
  (mho "graphicx")
  (upequal "graphicx")

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
  (chunk "ecltree")

  (url "hyperref")
  (href "hyperref")

  (citet "natbib")
  (citep "natbib")
  (citet* "natbib")
  (citep* "natbib")
  (citealt "natbib")
  (citealp "natbib")
  (citealt* "natbib")
  (citealp* "natbib")
  (citetext "natbib")
  (citeauthor "natbib")
  (citeauthor* "natbib")
  (citeyear "natbib"))

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
    (values (string->symbol s) arity)))

(tm-define (latex-arity tag)
  "Get the arity of a LaTeX @tag"
  (receive (s arity) (latex-resolve tag)
    (or arity 0)))

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
