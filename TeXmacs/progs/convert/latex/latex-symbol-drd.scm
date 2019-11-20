
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-symbol-drd.scm
;; DESCRIPTION : LaTeX symbols supported by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-symbol-drd)
  (:use (convert latex latex-command-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic symbols and big symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-symbol%
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
  diamond triangleleft triangleright land lor lnot
  oplus ominus otimes oslash odot bigcirc amalg notin

  ;; Relations
  leq le geq ge equiv models prec
  succ sim perp preceq succeq
  simeq mid ll gg asymp
  parallel subset supset approx bowtie
  subseteq supseteq cong
  ne neq smile sqsubseteq sqsupseteq
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
  ldots cdots vdots ddots hdots aleph
  prime forall infty hbar emptyset
  exists nabla surd triangle
  imath jmath ell neg
  top flat natural sharp wp
  bot clubsuit diamondsuit heartsuit spadesuit
  Re Im angle partial textbackslash
  dag ddag dagger ddagger guillemotleft guillemotright

  ;; Delimiters
  uparrow Uparrow downarrow Downarrow
  updownarrow Updownarrow
  lfloor rfloor lceil rceil
  langle rangle backslash

  ;; Big delimiters
  rmoustache lmoustache rgroup lgroup
  lbrack rbrack lbrace rbrace
  arrowvert Arrowvert bracevert)

(logic-group latex-big-symbol%
  sum prod coprod
  bignone bigtimes bigoplus bigotimes bigodot
  bigvee bigwedge bigsqcup bigcup bigcap bigpluscup
  bigtriangledown bigtriangleup
  int bigiint bigiiint bigiiiint bigidotsint oint bigoiint bigoiiint
  bigintwl bigiintwl bigiiintwl bigiiiintwl bigidotsintwl
  bigointwl bigoiintwl bigoiiintwl
  bigupint bigupiint bigupiiint bigupoint bigupoiint bigupoiiint
  bigupintwl bigupiintwl bigupiiintwl bigupointwl bigupoiintwl bigupoiiintwl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from latexsym package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-latexsym-symbol%
  mho Join Box Diamond leadsto
  sqsubset sqsupset lhd rhd unlhd unrhd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from amssymb package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-ams-symbol%
  ;;`
  ;; Box sqsubset sqsupset lhd unlhd rhd unrhd
  Bbbk Bumpeq Cap Cup Finv Game Lleftarrow Lsh
  Rrightarrow Rsh Subset Supset Vdash Vvdash
  angle approxeq backepsilon backprime backsim backsimeq barwedge
  because beth between bigstar blacklozenge blacksquare blacktriangle
  blacktriangledown blacktriangleleft blacktriangleright box boxdot
  boxminus boxplus boxtimes bumpeq centerdot checkmark circeq
  circlearrowleft circlearrowright circledR circledS circledast
  circledcirc circleddash complement curlyeqprec curlyeqsucc curlyvee
  curlywedge curvearrowleft curvearrowright daleth diagdown diagup
  digamma divideontimes doteqdot dotplus doublebarwedge downdownarrows
  downharpoonleft downharpoonright eqcirc eqsim eqslantgtr eqslantless
  eth fallingdotseq frown geqq geqslant ggg gimel gnapprox gneq gneqq
  gnsim gtrapprox gtrdot gtreqless gtreqqless gtrless gtrsim gvertneqq
  hslash intercal leftarrowtail leftleftarrows leftrightarrows
  leftrightharpoons leftrightsquigarrow leftthreetimes leqq leqslant
  lessapprox lessdot lesseqgtr lesseqqgtr lessgtr lesssim
  llcorner lll lnapprox lneq lneqq lnsim looparrowleft looparrowright
  lozenge lrcorner ltimes lvert lVert lvertneqq maltese measuredangle models
  multimap nLeftarrow nLeftrightarrow nRightarrow nVDash nVdash
  ncong nexists ngeq ngeqq ngeqslant ngtr nleftarrow nleftrightarrow
  nleq nleqq nleqslant nless nmid nparallel nprec npreceq nrightarrow
  nshortmid nshortparallel nsim nsubseteq nsubseteqq nsucc nsucceq
  nsupseteq nsupseteqq ntriangleleft ntrianglelefteq ntriangleright
  ntrianglerighteq nvDash nvdash pitchfork precapprox preccurlyeq
  precnapprox precneqq precnsim precsim propto rhd rightarrowtail
  rightleftarrows rightleftharpoons rightrightarrows rightsquigarrow
  rightthreetimes risingdotseq rtimes rvert rVert shortleftarrow shortmid
  shortparallel shortrightarrow smalldash smallfrown smallsetminus
  smallsmile smile sphericalangle subseteqq
  subsetneq subsetneqq succapprox succcurlyeq succnapprox succneqq
  succnsim succsim supseteqq supsetneq supsetneqq therefore
  thickapprox thicksim triangle triangledown trianglelefteq
  triangleq trianglerighteq twoheadleftarrow twoheadrightarrow
  ulcorner upharpoonleft upharpoonright upuparrows
  urcorner vDash varkappa varnothing varpropto varsubsetneq
  varsubsetneqq varsupsetneq varsupsetneqq vartriangle
  vartriangleleft vartriangleright veebar yen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from wasysym package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-wasy-symbol%
  agemO APLbox APLcomment APLdownarrowbox APLdown APLinput
  APLleftarrowbox APLrightarrowbox APLstar APLuparrowbox APLup apprge
  apprle aquarius ascnode ataribox bell blacksmiley
  Bowtie brokenvert cancer capricornus cent checked
  CIRCLE Circle clock conjunction currency davidsstar
  descnode dh diameter DOWNarrow eighthnote female
  frownie fullnote gemini halfnote hexagon hexstar
  invdiameter inve invneg jupiter kreuz LEFTarrow
  LEFTCIRCLE Leftcircle leftmoon leftturn libra logof
  male mercury neptune octagon openo opposition
  pentagon permil phone pisces pluto pointer
  quarternote recorder RIGHTarrow RIGHTCIRCLE Rightcircle
  rightmoon rightturn sagittarius saturn
  scorpio smiley square sun taurus Thorn
  thorn twonotes UParrow uranus varangle varhexagon
  varhexstar varlightning vernal VHF virgo
  ;;wasy-38 wasy-58 wasy-80 wasy-81 wasy-82
  wasyBox wasyDiamond wasyleadsto wasylhd wasylozenge
  wasypropto wasyrhd wasysqsubset wasysqsupset wasytherefore
  wasyunlhd wasyunrhd XBox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from stmaryrd package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-stmary-symbol%
  Arrownot arrownot baro bbslash binampersand bindnasrepma
  boxast boxbar boxbox boxbslash boxcircle
  ;;boxdot
  boxempty boxslash curlyveedownarrow curlyveeuparrow
  curlywedgedownarrow curlywedgeuparrow fatbslash fatsemi fatslash
  inplus interleave large-llbracket large-rrbracket Lbag lbag
  leftarrowtriangle leftrightarroweq leftrightarrowtriangle
  leftslice lightning llbracket llceil llfloor llparenthesis
  Mapsfromchar mapsfromchar Mapstochar merge minuso moo
  niplus nnearrow nnwarrow nplus ntrianglelefteqslant
  ntrianglerighteqslant obar oblong obslash ogreaterthan
  olessthan ovee owedge Rbag rbag rightarrowtriangle rightslice
  rrbracket rrceil rrfloor rrparenthesis shortdownarrow
  shortleftarrow shortrightarrow shortuparrow ssearrow sslash
  sswarrow subsetpluseq subsetplus supsetpluseq supsetplus talloblong
  trianglelefteqslant trianglerighteqslant varbigcirc varcurlyvee
  varcurlywedge varoast varobar varobslash varocircle
  varodot varogreaterthan varolessthan varominus varoplus varoslash
  varotimes varovee varowedge vartimes Ydown Yleft Yright Yup)

(logic-group latex-stmary-big-symbol%
  bigbox bigcurlyvee bigcurlywedge biginterleave
  bignplus bigparallel bigsqcap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from mathabx package
;; NOTE: we avoid using the mathabx package because it tends
;; to be badly installed and incompatible with certain styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(logic-group latex-mathabx-symbol%
;;  divides ndivides npreccurlyeq asterisk
;;  dottimes nequiv precdot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from textcomp package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-textcomp-symbol%
  textcent textcurrency textyen textbrokenbar textasciidieresis textlnot
  textasciimacron textdegree textpm texttwosuperior textthreesuperior
  textasciiacute textmu textonesuperior textonequarter textonehalf
  textthreequarters texttimes textdiv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols from upgreek package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group latex-upgreek-symbol%
  upalpha upbeta upgamma updelta upepsilon
  upvarepsilon upzeta upeta uptheta upvartheta
  upiota upkappa uplambda upmu upnu upomicron
  upxi uppi upvarpi uprho
  upvarrho upsigma upvarsigma uptau upupsilon
  upphi upvarphi upchi uppsi upomega

  Upalpha Upbeta Upgamma Updelta Upepsilon
  Upzeta Upeta Uptheta Upiota Upkappa Uplambda
  Upmu Upnu Upomicron Upxi Uppi Uprho Upsigma
  Uptau Upupsilon Upphi Upchi Uppsi Upomega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-rules
  ((latex-arity% 'x 0) (latex-symbol% 'x))
  ((latex-arity% 'x 0) (latex-big-symbol% 'x))
  ((latex-symbol% 'x) (latex-latexsym-symbol% 'x))
  ((latex-needs% 'x "latexsym") (latex-latexsym-symbol% 'x))
  ((latex-symbol% 'x) (latex-ams-symbol% 'x))
  ((latex-needs% 'x "amssymb") (latex-ams-symbol% 'x))
  ((latex-symbol% 'x) (latex-wasy-symbol% 'x))
  ((latex-needs% 'x "wasysym") (latex-wasy-symbol% 'x))
  ((latex-symbol% 'x) (latex-stmary-symbol% 'x))
  ((latex-needs% 'x "stmaryrd") (latex-stmary-symbol% 'x))
  ((latex-big-symbol% 'x) (latex-stmary-big-symbol% 'x))
  ((latex-needs% 'x "stmaryrd") (latex-stmary-big-symbol% 'x))
  ;;((latex-symbol% 'x) (latex-mathabx-symbol% 'x))
  ;;((latex-needs% 'x "mathabx") (latex-mathabx-symbol% 'x))
  ((latex-symbol% 'x) (latex-textcomp-symbol% 'x))
  ((latex-needs% 'x "textcomp") (latex-textcomp-symbol% 'x))
  ((latex-symbol% 'x) (latex-upgreek-symbol% 'x))
  ((latex-needs% 'x "upgreek") (latex-upgreek-symbol% 'x)))
