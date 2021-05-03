
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-command-drd.scm
;; DESCRIPTION : Formal specification of standard LaTeX commands
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-command-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Any LaTeX tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-rules
  ((latex-tag% 'x) (latex-arity% 'x 'y))
  ((latex-supports-option% 'x #t) (latex-optional-arg% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-command-0%
  ,(string->symbol " ") ,(string->symbol ";") 
  ,(string->symbol ",") ,(string->symbol ":") 
  - / [ ] ! * ,(string->symbol "|") i j ss SS oe OE ae AE
  AA DH L NG O S TH aa dh dj l ng o P th pounds colon and lq rq
  quad qquad enspace thinspace par smallskip medskip bigskip
  noindent newline linebreak nobreak nolinebreak strut
  pagebreak nopagebreak newpage newdoublepage clearpage cleardoublepage
  newblock bgroup egroup protect cr hfil hfill hfilll appendix limits nolimits
  dots maketitle tableofcontents TeX LaTeX onecolumn twocolumn
  begingroup endgroup printindex today bmod toprule midrule bottomrule

  ;; AMS commands
  dotsc dotsb dotsm dotsi dotso qed
  ;; mathtools
  coloneqq
  ;; temporarily
  hline hrulefill
  ;; rewritten
  notin vert Vert addots
  implies iff gets
  ;; wikipedia
  infin rang
  ;; bibtex
  bysame
  ;; for (e.g.) includegraphics
  width height
  ;; miscellaneous
  null unskip

  ;; Algorithms
  AND BlankLine Ensure ENSURE FALSE GLOBALS NOT OR PRINT Require REQUIRE
  Repeat RETURN State STATE TO KwTo TRUE XOR Else ENDBODY EndFor ENDFOR
  EndFunction EndIf ENDIF ENDINPUTS EndLoop ENDLOOP ENDOUTPUTS
  EndProcedure ENDWHILE EndWhile Loop)

(logic-group latex-command-1%
  part* chapter* section* subsection* subsubsection* paragraph* subparagraph*
  nextbib geometry
  footnote overline underline <sub> <sup> not left middle right
  big Big bigg Bigg bigl Bigl biggl Biggl
  bigm Bigm biggm Biggm bigr Bigr biggr Biggr
  bar Bar hat Hat tilde Tilde widehat widetilde vec Vec bm ring
  overrightarrow overleftarrow overleftrightarrow
  underrightarrow underleftarrow underleftrightarrow
  grave Grave acute Acute check Check breve Breve invbreve abovering mathring
  dot Dot ddot Ddot dddot ddddot mod pod pmod
  label ref pageref index hspace hspace* vspace vspace* mspace
  mbox hbox textnormal text not substack
  ,(string->symbol "'") ,(string->symbol "`") ,(string->symbol "\"")
  ^ over atop choose ~ = u v H t c d b k r textsuperscript textsubscript
  thispagestyle ensuremath
  mathord mathbin mathopen mathpunct mathop mathrel mathclose mathalpha
  mathinner
  arabic alph Alph roman Roman fnsymbol displaylines cases underbrace overbrace
  phantom hphantom vphantom smash date terms
  newcounter stepcounter refstepcounter value
  citealt citealt* citealp*
  citetext citeauthor citeauthor* citeyear onlinecite citeN
  epsfig url penalty centerline fbox framebox cline cmidrule
  enlargethispage
  newlength newdimen newskip
  Comment COMMENT For ForAll If Input KwData KwResult KwRet lnl nllabel
  lElse uElse Output Until UNTIL While
  etalchar MR listpart custombinding cref Cref)

(logic-group latex-command-1% ;; . needs a special treatment
  ,(string->symbol "."))

(logic-group latex-command-2%
  binom tbinom dbinom cfrac tfrac equal href
  sideset stackrel underaccent
  setcounter addtocounter setlength addtolength
  colorbox scalebox texorpdfstring raisebox foreignlanguage
  Call Function Procedure SetKw SetKwData SetKwFunction SetKwInOut
  ifthispageodd adjustbox)

(logic-group latex-command-3%
  ifthenelse resizebox fcolorbox @setfontsize eIf multicolumn)

(logic-group latex-command-4%
  mathchoice)

(logic-group latex-command-6%
  genfrac @startsection)

(logic-rules
  ((latex-command% 'x) (latex-command-0% 'x))
  ((latex-arity% 'x 0) (latex-command-0% 'x))
  ((latex-command% 'x) (latex-command-1% 'x))
  ((latex-arity% 'x 1) (latex-command-1% 'x))
  ((latex-command% 'x) (latex-command-2% 'x))
  ((latex-arity% 'x 2) (latex-command-2% 'x))
  ((latex-command% 'x) (latex-command-3% 'x))
  ((latex-arity% 'x 3) (latex-command-3% 'x))
  ((latex-command% 'x) (latex-command-4% 'x))
  ((latex-arity% 'x 4) (latex-command-4% 'x))
  ((latex-command% 'x) (latex-command-6% 'x))
  ((latex-arity% 'x 6) (latex-command-6% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX commands with optional arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-command-0*%
  item ,(string->symbol "\\")
  BODY ELSE INPUTS LOOP OUTPUTS REPEAT
  hdashline)

(logic-group latex-command-1*%
  usepackage documentclass documentstyle sqrt bibitem cite caption  
  title author thanks marginpar
  part chapter section subsection subsubsection paragraph subparagraph
  includegraphics includegraphics*
  makebox
  subjclass declaretheorem footnotetext
  xleftarrow xrightarrow xleftrightarrow xminus
  xLeftarrow xRightarrow xLeftrightarrow xequal
  xmapsto xmapsfrom citealp citet citep citet* citep*
  Begin ELSIF FORALL FOR IF WHILE tcp tcp* tcc tcc*
  hyperref)

(logic-group latex-command-2*%
  def newcommand renewcommand providecommand
  newtheorem newtheorem* frac parbox 
  ElseIf uElseIf lElseIf ForEach lForEach lForAll lFor)

(logic-group latex-command-3*%
  category newenvironment renewenvironment multirow)

(logic-rules
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

(logic-group latex-environment-0%
  begin-document begin-abstract begin-verbatim begin-proof
  begin-matrix begin-pmatrix begin-bmatrix begin-vmatrix begin-smallmatrix
  begin-cases
  begin-center begin-flushleft begin-flushright
  begin-picture)

(logic-group latex-environment-0*%
  begin-figure begin-table begin-figure* begin-table*
  begin-algorithmic begin-algorithm begin-algorithm2e
  begin-teaserfigure)

(logic-group latex-environment-1%
  begin-otherlanguage begin-otherlanguage*
  begin-tabbing begin-thebibliography begin-multicols)

(logic-group latex-environment-1*%
  begin-array begin-tabular begin-minipage)

(logic-group latex-environment-2*%
  begin-tabular* begin-tabularx)

(logic-rules
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
  ((latex-optional-arg% 'x) (latex-environment-1*% 'x))
  ((latex-environment-2% 'x) (latex-environment-2*% 'x))
  ((latex-optional-arg% 'x) (latex-environment-2*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enunciations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-enunciation%
  begin-theorem begin-proposition begin-lemma begin-corollary begin-proof
  begin-axiom begin-definition begin-notation begin-conjecture
  begin-remark begin-note begin-example begin-warning
  begin-convention begin-acknowledgments
  begin-exercise begin-problem
  begin-solution begin-question begin-answer
  begin-quote-env begin-quotation begin-verse

  begin-theorem* begin-proposition* begin-lemma* begin-corollary*
  begin-axiom* begin-definition* begin-notation* begin-conjecture*
  begin-remark* begin-note* begin-example* begin-warning*
  begin-convention* begin-acknowledgments*
  begin-exercise* begin-problem*
  begin-solution* begin-question* begin-answer*

  ;; guessed
  begin-th begin-thm begin-prop begin-lem begin-cor begin-corr
  begin-pf begin-dem begin-preuve begin-IEEEproof
  begin-ax begin-def begin-dfn begin-defn
  begin-not begin-ex begin-exa begin-rem begin-war begin-conv
  begin-exe begin-exc begin-exo begin-prop begin-sol begin-ans
  begin-acks)

(logic-rules
  ((latex-environment-0*% 'x) (latex-enunciation% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-modifier-0%
  normalfont rm tt sf md bf it em sl sc rmfamily ttfamily sffamily
  mdseries bfseries upshape itshape slshape scshape
  displaystyle textstyle scriptstyle scriptscriptstyle cal frak Bbb boldmath
  tiny scriptsize footnotesize small normalsize
  large Large LARGE huge Huge
  black white grey red blue yellow green orange magenta brown pink
  centering raggedleft raggedright flushleft flushright)

(logic-group latex-modifier-1%
  textnormalfont
  textrm texttt textsf textmd textbf textup textit textsl textsc emph
  mathrm mathtt mathsf mathmd mathbf mathup mathit mathsl mathnormal
  mathcal mathfrak mathbb mathbbm mathscr operatorname boldsymbol
  lowercase MakeLowercase uppercase MakeUppercase selectlanguage)

(logic-group latex-modifier-1*%
  color)

(logic-group latex-modifier-2*%
  textcolor)

(logic-rules
  ((latex-modifier% 'x)     (latex-modifier-0% 'x))
  ((latex-arity% 'x 0)      (latex-modifier-0% 'x))
  ((latex-modifier% 'x)     (latex-modifier-1% 'x))
  ((latex-arity% 'x 1)      (latex-modifier-1% 'x))
  ((latex-optional-arg% 'x) (latex-modifier-1*% 'x))
  ((latex-modifier% 'x)     (latex-modifier-1*% 'x))
  ((latex-arity% 'x 1)      (latex-modifier-1*% 'x))
  ((latex-optional-arg% 'x) (latex-modifier-2*% 'x))
  ((latex-modifier% 'x)     (latex-modifier-2*% 'x))
  ((latex-arity% 'x 2)      (latex-modifier-2*% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special types of LaTeX primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-control%
  $ & % ,(string->symbol "#") _ { } <less> <gtr>)

(logic-group latex-operator%
  arccos arcsin arctan arg cos cosh cot coth csc deg det dim exp gcd hom
  inf ker lg lim liminf limsup ln log max min Pr sec sin sinh sup tan tanh)

(logic-group latex-list%
  begin-itemize begin-enumerate begin-description
  begin-asparaitem begin-inparaitem begin-compactitem
  begin-asparaenum begin-inparaenum begin-compactenum)

(logic-group latex-math-environment-0%
  begin-formula begin-equation*
  begin-math begin-displaymath begin-equation
  begin-eqnarray begin-eqnarray*
  begin-flalign begin-flalign*
  begin-align begin-align*
  begin-multline begin-multline*
  begin-gather begin-gather*
  begin-eqsplit begin-eqsplit*)

(logic-group latex-math-environment-1%
  begin-alignat begin-alignat*)

(logic-rules
  ((latex-arity% 'x 0) (latex-control% 'x))
  ((latex-arity% 'x 0) (latex-operator% 'x))
  ((latex-environment-0*% 'x) (latex-list% 'x))
  ((latex-math-environment% 'x) (latex-math-environment-0% 'x))
  ((latex-math-environment% 'x) (latex-math-environment-1% 'x))
  ((latex-environment-1% 'x) (latex-math-environment-1% 'x))
  ((latex-environment-0% 'x) (latex-math-environment-0% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-counter%
  badness enumi enumii enumiii enumiv equation figure inputlineno
  mpfootnote page setlanguage table)

(logic-rules
  ((latex-arity% 'x 0) (latex-counter% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-name%
  abstractname appendixname contentname figurename indexname
  litfigurename littablename partname refname tablename)

(logic-rules
  ((latex-arity% 'x 0) (latex-name% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lengths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-length%
  ;; From latex.ltx
  ;; -- lengths
  @textfloatsheight arraycolsep arrayrulewidth columnsep columnseprule
  columnwidth doublerulesep emergencystretch evensidemargin fboxrule
  fboxsep footnotesep footskip headheight headsep itemindent labelsep
  labelwidth leftmargin leftmargini leftmarginii leftmarginiii
  leftmarginiv leftmarginv leftmarginvi linewidth listparindent
  marginparpush marginparsep marginparwidth oddsidemargin p@ paperheight
  paperwidth rightmargin tabbingsep tabcolsep textheight textwidth
  topmargin unitlength z@ @bls @vpt @vipt @viipt @viiipt @ixpt @xpt @xipt
  @xiipt @xivpt @xviipt @xxpt @xxvpt 
  ;; -- skips
  topsep partopsep itemsep parsep floatsep textfloatsep intextsep
  dblfloatsep dbltextfloatsep 
  ;; From latex classes
  abovecaptionskip belowcaptionskip bibindent
  ;; From fleqn
  mathindent
  ;; Plain TeX
  maxdimen hfuzz vfuzz overfullrule hsize vsize maxdepth lineskiplimit
  delimitershortfall nulldelimiterspace scriptspace mathsurround
  predisplaysize displaywidth displayindent parindent hangindent hoffset
  voffset baselineskip lineskip parskip abovedisplayskip
  abovedisplayshortskip belowdisplayskip belowdisplayshortskip leftskip
  rightskip topskip splittopskip tabskip spaceskip xspaceskip parfillskip
  thinmuskip medmuskip thickmuskip hideskip smallskipamount medskipamount
  bigskipamount normalbaselineskip normallineskip normallineskiplimit jot 
  )

(logic-rules
  ((latex-arity% 'x 0) (latex-length% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be imported as pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-as-pic-0%
  begin-pspicture begin-pspicture* begin-tikzpicture)

(logic-group latex-as-pic-1%
  xymatrix)

(logic-rules
  ((latex-as-pic% 'x)       (latex-as-pic-0% 'x))
  ((latex-as-pic% 'x)       (latex-as-pic-1% 'x))
  ((latex-arity%  'x 0)     (latex-as-pic-0% 'x))
  ((latex-arity%  'x 1)     (latex-as-pic-1% 'x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be ignored
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-ignore-0%
  allowbreak notag xspace break sloppy makeatother makeatletter relax
  qedhere
  ignorespacesafterend ignorespaces balancecolumns
  tightlist)

(logic-group latex-ignore-0*%
  displaybreak allowdisplaybreaks)

(logic-group latex-ignore-1%
  tag hyphenation)

(logic-group latex-ignore-2%
  newdir)

(logic-rules
  ((latex-ignore% 'x)       (latex-ignore-0% 'x))
  ((latex-ignore% 'x)       (latex-ignore-0*% 'x))
  ((latex-ignore% 'x)       (latex-ignore-1% 'x))
  ((latex-ignore% 'x)       (latex-ignore-2% 'x))
  ((latex-arity% 'x 0)      (latex-ignore-0% 'x))
  ((latex-arity% 'x 0)      (latex-ignore-0*% 'x))
  ((latex-arity% 'x 1)      (latex-ignore-1% 'x))
  ((latex-arity% 'x 2)      (latex-ignore-2% 'x))
  ((latex-optional-arg% 'x) (latex-ignore-1*% 'x)))
