
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

(texmacs-module (convert latex latex-symbol-drd))

(drd-group latex-ams-symbol%
  ;;`
  Bbbk Box Bumpeq Cap Cup Finv Game Lleftarrow Lsh
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
  lessapprox lessdot lesseqgtr lesseqqgtr lessgtr lesssim lhd
  llcorner lll lnapprox lneq lneqq lnsim looparrowleft looparrowright
  lozenge lrcorner ltimes lvertneqq maltese measuredangle models
  multimap nLeftarrow nLeftrightarrow nRightarrow nVDash nVdash
  ncong nexists ngeq ngeqq ngeqslant ngtr nleftarrow nleftrightarrow
  nleq nleqq nleqslant nless nmid nparallel nprec npreceq nrightarrow
  nshortmid nshortparallel nsim nsubseteq nsubseteqq nsucc nsucceq
  nsupseteq nsupseteqq ntriangleleft ntrianglelefteq ntriangleright
  ntrianglerighteq nvDash nvdash pitchfork precapprox preccurlyeq
  precnapprox precneqq precnsim precsim propto rhd rightarrowtail
  rightleftarrows rightleftharpoons rightrightarrows rightsquigarrow
  rightthreetimes risingdotseq rtimes shortleftarrow shortmid
  shortparallel shortrightarrow smalldash smallfrown smallsetminus
  smallsmile smile sphericalangle sqsubset sqsupset subseteqq
  subsetneq subsetneqq succapprox succcurlyeq succnapprox succneqq
  succnsim succsim supseteqq supsetneq supsetneqq therefore
  thickapprox thicksim triangle triangledown trianglelefteq
  triangleq trianglerighteq twoheadleftarrow twoheadrightarrow
  ulcorner unlhd unrhd upharpoonleft upharpoonright upuparrows
  urcorner vDash varkappa varnothing varpropto varsubsetneq
  varsubsetneqq varsupsetneq varsupsetneqq vartriangle
  vartriangleleft vartriangleright veebar yen)

(drd-group latex-wasy-symbol%
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

(drd-group latex-stmary-symbol%
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

(drd-group latex-mathabx-symbols%
  divides ndivides)

(drd-rules
  ((latex-needs% 'x "amssymb") (latex-ams-symbol% 'x))
  ((latex-needs% 'x "wasysym") (latex-wasy-symbol% 'x))
  ((latex-needs% 'x "stmaryrd") (latex-stmary-symbol% 'x))
  ((latex-needs% 'x "mathabx") (latex-mathabx-symbol% 'x)))
