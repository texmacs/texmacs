
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-symbol-drd.scm
;; DESCRIPTION : LaTeX symbols supported by TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (drd latex latex-symbol-drd))

(drd-group latex-ams-symbol%
  ;;`
  angle	backprime backsim
  backsimeq barwedge because
  between blacklozenge blacksquare
  blackstar blacktriangledown blacktriangleleft
  blacktriangleright blacktriangleup Box
  box boxdot boxminus
  boxplus boxtimes Bumpeq
  bumpeq Cap centerdot
  checkmark circeq circlearrowleft
  circlearrowright circledast circledcirc
  circledminus circledR circledS
  complement Cup curlyeqprec
  curlyeqsucc curlyvee curlywedge
  doteqdot dotplus doublebarwedge
  downdownarrows downharpoonleft downharpoonright
  eqcirc eqslantgtr eqslantless
  fallingdoteq frown geqq
  geqslant ggg gtrapprox
  gtreqless gtreqqless gtrless
  gtrsim intercal leftarrowtail
  leftleftarrows leftrightarrows leftrightharpoons
  leftrightsquigarrow leftthreetimes leqq
  leqslant lessapprox lesseqgtr
  lesseqqgtr lessgtr lesssim
  lhd llcorner Lleftarrow
  lll looparrowleft looparrowright
  lozenge lrcorner Lsh
  maltese measuredangle models
  multimap pitchfork preccurlyeq
  precsim propto rhd
  rightarrowtail rightleftarrows rightleftharpoons
  rightrightarrows rightsquigarrow rightthreetimes
  risingdoteq Rrightarrow Rsh
  shortleftarrow shortrightarrow smalldash
  smile sphericalangle sqsubset
  sqsupset Subset subseteqq
  succcurlyeq succsim Supset
  supseteqq therefore triangle
  triangledown trianglelefteq triangleq
  trianglerighteq twoheadleftarrow twoheadrightarrow
  ulcorner unlhd unrhd
  upharpoonleft upharpoonright upuparrows
  urcorner vartriangleleft vartriangleright
  Vdash vDash veebar
  Vvdash yen)

(drd-group latex-wasy-symbol%
  agemO APLbox APLcomment
  APLdownarrowbox APLdown APLinput
  APLleftarrowbox APLrightarrowbox APLstar
  APLuparrowbox APLup apprge
  apprle aquarius ascnode
  ataribox bell blacksmiley
  Bowtie brokenvert cancer
  capricornus cent checked
  CIRCLE Circle clock
  conjunction currency davidsstar
  descnode dh diameter
  DOWNarrow eighthnote female
  frownie fullnote gemini
  halfnote hexagon hexstar
  invdiameter inve invneg
  jupiter kreuz LEFTarrow
  LEFTCIRCLE Leftcircle leftmoon
  leftturn libra logof
  male mercury neptune
  octagon openo opposition
  pentagon permil phone
  pisces pluto pointer
  quarternote recorder RIGHTarrow
  RIGHTCIRCLE Rightcircle rightmoon
  rightturn sagittarius saturn
  scorpio smiley square
  sun taurus Thorn
  thorn twonotes UParrow
  uranus varangle varhexagon
  varhexstar varlightning vernal
  VHF virgo
  ;;wasy-38
  ;;wasy-58
  ;;wasy-80
  ;;wasy-81
  ;;wasy-82
  wasyBox wasyDiamond
  wasyleadsto wasylhd wasylozenge
  wasypropto wasyrhd wasysqsubset
  wasysqsupset wasytherefore wasyunlhd
  wasyunrhd XBox)

(drd-group latex-stmary-symbol%
  Arrownot arrownot baro
  bbslash binampersand bindnasrepma
  boxast boxbar boxbox
  boxbslash boxcircle
  ;;boxdot
  boxempty boxslash curlyveedownarrow
  curlyveeuparrow curlywedgedownarrow curlywedgeuparrow
  fatbslash fatsemi fatslash
  inplus interleave large-llbracket
  large-rrbracket Lbag lbag
  leftarrowtriangle leftrightarroweq leftrightarrowtriangle
  leftslice lightning llbracket
  llceil llfloor llparenthesis
  Mapsfromchar mapsfromchar Mapstochar
  merge minuso moo
  niplus nnearrow nnwarrow
  nplus ntrianglelefteqslant ntrianglerighteqslant
  obar oblong obslash
  ogreaterthan olessthan ovee
  owedge Rbag rbag
  rightarrowtriangle rightslice rrbracket
  rrceil rrfloor rrparenthesis
  shortdownarrow shortleftarrow shortrightarrow
  shortuparrow ssearrow sslash
  sswarrow subsetpluseq subsetplus
  supsetpluseq supsetplus talloblong
  trianglelefteqslant trianglerighteqslant varbigcirc
  varcurlyvee varcurlywedge varoast
  varobar varobslash varocircle
  varodot varogreaterthan varolessthan
  varominus varoplus varoslash
  varotimes varovee varowedge
  vartimes Ydown Yleft
  Yright Yup)

(drd-rules
  ((latex-needs% 'x "amssymb") (latex-ams-symbol% 'x))
  ((latex-needs% 'x "wasysym") (latex-wasy-symbol% 'x))
  ((latex-needs% 'x "stmaryrd") (latex-stmary-symbol% 'x)))
