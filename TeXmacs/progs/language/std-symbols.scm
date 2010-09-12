
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-symbols.scm
;; DESCRIPTION : standard mathematical symbols
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language std-symbols))

(define-language std-symbols
  (Assign-symbol
   "<assign>" "<plusassign>" "<minusassign>" "<astassign>" "<overassign>")
  
  (Flux-symbol
   "<lflux>" "<gflux>")

  (Models-symbol
   "<models>" "<vdash>" "<dashv>" "<vDash>" "<Vdash>" "<Vvdash>" "<VDash>"
   "<longvdash>" "<longdashv>" "<longvDash>"
   "<longVdash>" "<longVvdash>" "<longVDash>"
   "<nvdash>" "<ndashv>" "<nvDash>" "<nVdash>" "<nVvdash>" "<nVDash>")

  (Quantifier-symbol
   "<forall>" "<exists>" "<nexists>")

  (Imply-nolim-symbol
   "<implies>" "<equivalent>" "<Leftarrow>" "<Rightarrow>" "<Leftrightarrow>"
   "<Longleftarrow>" "<Longrightarrow>" "<Longleftrightarrow>"
   "<Lleftarrow>" "<Rrightarrow>"
   "<nLeftarrow>" "<nRightarrow>" "<nLeftrightarrow>")

  (Imply-lim-symbol
   "<Leftarrowlim>" "<Rightarrowlim>" "<Leftrightarrowlim>"
   "<Longleftarrowlim>" "<Longrightarrowlim>" "<Longleftrightarrowlim>")

  (Imply-symbol
   Imply-nolim-symbol Imply-symbol)

  (Or-symbol
   "<vee>" "<curlyvee>")

  (And-symbol
   "<wedge>" "<curlywedge>")

  (Not-symbol
   "<neg>")

  (Relation-nolim-symbol
   "=" "<ne>" "<neq>" "<longequal>" "<less>" "<gtr>" "<le>" "<leq>"
   "<prec>" "<preceq>" "<ll>" "<lleq>" "<subset>" "<subseteq>"
   "<sqsubset>" "<sqsubseteq>" "<in>" "<ge>" "<geq>" "<succ>" "<succeq>"
   "<gg>" "<ggeq>" "<supset>" "<supseteq>" "<sqsupset>" "<sqsupseteq>" "<ni>"
   "<equiv>" "<nequiv>" "<sim>" "<simeq>" "<asymp>" "<approx>" "<cong>"
   "<subsetsim>" "<supsetsim>" "<doteq>" "<propto>" "<varpropto>"
   "<perp>" "<bowtie>" "<Join>" "<smile>" "<frown>" "<signchange>"
   "<mid>" "<parallel>" "<shortmid>" "<shortparallel>" "<nmid>"
   "<nparallel>" "<nshortmid>" "<nshortparallel>"

   "<approxeq>" "<backsim>" "<backsimeq>" "<Bumpeq>" "<bumpeq>" "<circeq>"
   "<curlyeqprec>" "<curlyeqsucc>" "<Doteq>" "<doteqdot>" "<eqcirc>"
   "<eqslantgtr>" "<eqslantless>" "<fallingdotseq>" "<geqq>" "<geqslant>"
   "<ggg>" "<gggtr>" "<gnapprox>" "<gneq>" "<gneqq>" "<gnsim>" "<gtrapprox>"
   "<gtrdot>" "<gtreqdot>" "<gtreqless>" "<gtreqqless>" "<gtrless>"
   "<gtrsim>" "<gvertneqq>" "<leqq>" "<leqslant>" "<lessapprox>"
   "<lessdot>" "<lesseqdot>" "<lesseqgtr>" "<lesseqqgtr>" "<lessgtr>"
   "<lesssim>" "<lll>" "<llless>" "<lnapprox>" "<lneq>" "<lneqq>"
   "<lnsim>" "<lvertneqq>" "<napprox>" "<ngeq>" "<ngeqq>" "<ngeqslant>"
   "<ngtr>" "<nleq>" "<nleqq>" "<nleqslant>" "<nless>" "<nprec>" "<npreceq>"
   "<nsim>" "<nasymp>" "<nsubset>" "<nsupset>" "<nsqsubset>" "<nsqsupset>"
   "<nsqsubseteq>" "<nsqsupseteq>" "<nsubseteq>" "<nsucc>" "<nsucceq>"
   "<nsupseteq>" "<nsupseteqq>" "<nvdash>" "<precapprox>" "<preccurlyeq>"
   "<npreccurlyeq>" "<precnapprox>" "<precneqq>" "<precnsim>" "<risingdoteq>"
   "<Subset>" "<subseteqq>" "<subsetneq>" "<subsetneqq>" "<succapprox>"
   "<succcurlyeq>" "<nsucccurlyeq>" "<succnapprox>" "<succneqq>"
   "<succnsim>" "<succsim>" "<Supset>" "<supseteqq>" "<supsetneq>"
   "<thickapprox>" "<thicksim>" "<varsubsetneq>" "<varsubsetneqq>"
   "<varsupsetneq>" "<varsupsetneqq>"

   "<vartriangleleft>" "<vartriangleright>" "<triangleleft>" "<triangleright>"
   "<trianglelefteq>" "<trianglerighteq>" "<trianglelefteqslant>"
   "<trianglerighteqslant>" "<blacktriangleleft>" "<blacktriangleright>"
   "<ntriangleleft>" "<ntriangleright>" "<ntrianglelefteq>" "<ntrianglerighteq>"
   "<ntrianglelefteqslant>" "<ntrianglerighteqslant>"

   "<precprec>" "<precpreceq>" "<precprecprec>" "<precprecpreceq>"
   "<succsucc>" "<succsucceq>" "<succsuccsucc>" "<succsuccsucceq>"
   "<nprecprec>" "<nprecpreceq>" "<nprecprecprec>" "<nprecprecpreceq>"
   "<nsuccsucc>" "<nsuccsucceq>" "<nsuccsuccsucc>" "<nsuccsuccsucceq>"
   "<asympasymp>" "<nasympasymp>" "<simsim>" "<nsimsim>" "<nin>" "<nni>"
   "<notin>" "<notni>" "<precdot>" "<preceqdot>" "<dotsucc>" "<dotsucceq>")

  (Relation-lim-symbol
   "<equallim>" "<longequallim>")

  (Relation-symbol
   Relation-nolim-symbol Relaton-symbol)

  (Arrow-nolim-symbol
   "<to>" "<into>" "<from>" "<transtype>" "<leftarrow>" "<rightarrow>"
   "<leftrightarrow>" "<mapsto>" "<mapsfrom>"
   "<hookleftarrow>" "<hookrightarrow>"
   "<longleftarrow>" "<longrightarrow>" "<longleftrightarrow>"
   "<longmapsto>" "<longmapsfrom>"
   "<longhookleftarrow>" "<longhookrightarrow>" "<leftharpoonup>"
   "<leftharpoondown>" "<rightleftharpoons>" "<rightharpoonup>"
   "<rightharpoondown>" "<leadsto>" "<noarrow>" "<searrow>" "<swarrow>"
   "<nwarrow>" "<longtwoheadleftarrow>" "<longtwoheadrightarrow>" "<leftprec>"
   "<leftpreceq>" "<succright>" "<succeqright>"

   "<circlearrowleft>" "<circlearrowdown>"
   "<curvearrowleft>" "<curvearrowright>"
   "<downdownarrows>" "<downharpoonleft>" "<downharpoonright>"
   "<leftarrowtail>" "<leftleftarrows>" "<leftrightarrows>"
   "<leftrightharpoons>"
   "<looparrowleft>" "<looparrowright>" "<Lsh>" "<multimap>"
   "<nleftarrow>" "<nleftrightarrow>" "<nrightarrow>" "<restriction>"
   "<rightarrowtail>" "<rightleftarrows>" "<rightleftharpoons>"
   "<rightrightarrows>" "<rightsquigarrow>" "<Rsh>" "<twoheadleftarrow>"
   "<twoheadrightarrow>" "<upharpoonleft>" "<upharpoonright>" "<upuparrows>")
  
  (Arrow-lim-symbol
   "<leftarrowlim>" "<rightarrowlim>" "<leftrightarrowlim>"
   "<longleftarrowlim>" "<longrightarrowlim>" "<longleftrightarrowlim>"
   "<mapstolim>" "<longmapstolim>"
   "<leftsquigarrowlim>" "<rightsquigarrowlim>" "<leftrightsquigarrowlim>")

  (Arrow-symbol
   Arrow-nolim-symbol Arrow-lim-symbol)

  (Prefix-operator
   "arccos" "arcsin" "arctan" "cos" "cosh" "cot" "coth" "csc"
   "deg" "det" "dim" "exp" "gcd" "hom" "ker" "Pr"
   "lg" "ln""log" "sec" "sin" "sinh" "tan" "tanh")

  (Infix-operator
   "div" "mod")

  (Big-operator
   "inf" "lim" "liminf" "limsup" "max" "min" "sup")

  (Big-nolim-symbol
   "<big-int>" "<big-oint>")

  (Big-lim-symbol
   "<big-sum>" "<big-prod>" "<big-amalg>" "<big-intlim>" "<big-ointlim>"
   "<big-cap>" "<big-cup>" "<big-sqcup>" "<big-vee>" "<big-wedge>"
   "<big-odot>" "<big-otimes>" "<big-oplus>" "<big-uplus>"
   "<big-triangleup>" "<big-triangledown>")

  (Big-symbol
   Big-nolim-symbol Big-lim-symbol)

  (Set-symmetric-symbol
   "<cap>" "<cup>" "<Cap>" "<Cup>" "<doublecap>" "<doublecup>")

  (Set-minus-symbol
   "<setminus>" "<smallsetminus>")

  (Plus-symbol
   "+" "<amalg>" "<oplus>" "<boxplus>" "<dotplus>" "<dotamalg>" "<dotoplus>")

  (Minus-symbol
   "-" "<pm>" "<mp>" "<ominus>" "<boxminus>")

  (Regular-times-symbol
   "<cdot>" "<times>" "<otimes>" "<circ>" "<boxdot>" "<boxtimes>"
   "<dottimes>" "<dototimes>" "<ltimes>" "<rtimes>" "<atimes>" "<btimes>"
   "<join>" "<ast>" "<star>" "<oast>")

  (Invisible-times-symbol
   "*")

  (Times-symbol
   Regular-times-symbol Invisible-times-symbol)

  (Regular-over-symbol
   "<oover>")

  (Condensed-over-symbol
   "/")

  (Over-symbol
   Regular-over-symbol Condensed-over-symbol)

  (Prefix-symbol
   "<um>" "<upl>" "<upm>" "<ump>" "<card>")

  (Power-symbol
   "^")

  (Postfix-symbol
   "!")

  (Open-symbol
   "(" "[" "{" "<lfloor>" "<lceil>" "<langle>"
   "<left-(>" "<left-[>" "<left-{>" "<left-less>"
   "<left-}>" "<left-]>" "<left-)>" "<left-gtr>"
   "<left-|>" "<left-||>" "<left-.>"
   "<left-lfloor>" "<left-lceil>" "<left-rfloor>" "<left-rceil>"
   "<left-langle>" "<left-rangle>")
  
  (Ponctuation-symbol
   "," ";" ":")

  (Bar-symbol
   "|" "<||>" "<mid-|>" "<mid-||>")

  (Close-symbol
   "}" "]" ")" "<rfloor>" "<rceil>" "<rangle>"
   "<right-(>" "<right-[>" "<right-{>" "<right-less>"
   "<right-}>" "<right-]>" "<right-)>" "<right-gtr>"
   "<right-|>" "<right-||>" "<right-.>"
   "<right-lfloor>" "<right-lceil>" "<right-rfloor>" "<right-rceil>"
   "<right-langle>" "<right-rangle>")

  (Suspension-nolim-symbol
   "<ldots>" "<cdots>" "<udots>" "<vdots>" "<ddots>" "<mdots>" "<colons>")
  
  (Suspension-lim-symbol
   "<cdotslim>")

  (Suspension-symbol
   Suspension-nolim-symbol Suspension-lim-symbol)
  
  (Spacing-symbol
   "<spc>")
  
  (Basic-symbol
   "<alpha>" "<beta>" "<gamma>" "<delta>" "<varepsilon>"
   "<epsilon>" "<zeta>" "<eta>" "<theta>" "<iota>"
   "<kappa>" "<lambda>" "<mu>" "<nu>" "<xi>" "<omikron>"
   "<varpi>" "<pi>" "<rho>" "<sigma>" "<tau>" "<upsilon>"
   "<varphi>" "<phi>" "<psi>" "<chi>" "<omega>"

   "<Alpha>" "<Beta>" "<Gamma>" "<Delta>" "<Varepsilon>"
   "<Epsilon>" "<Zeta>" "<Eta>" "<Theta>" "<Iota>"
   "<Kappa>" "<Lambda>" "<Mu>" "<Nu>" "<Xi>" "<Omikron>"
   "<Varpi>" "<Pi>" "<Rho>" "<Sigma>" "<Tau>" "<Upsilon>"
   "<Varphi>" "<Phi>" "<Psi>" "<Chi>" "<Omega>"

   "<ldot>" "<udot>"

   "<uparrow>" "<Uparrow>" "<downarrow>" "<Downarrow>"
   "<updownarrow>" "<Updownarrow>" "<mapsup>" "<mapsdown>"
   "<hookuparrow>" "<hookdownarrow>" "<longuparrow>" "<Longuparrow>"
   "<longdownarrow>" "<Longdownarrow>" "<longupdownarrow>" "<Longupdownarrow>"
   "<longmapsup>" "<longmapsdown>" "<longhookuparrow>" "<longhookdownarrow>"

   "<aleph>" "<hbar>" "<imath>" "<jmath>" "<ell>"
   "<wp>" "<Re>" "<Im>" "<Mho>" "<prime>" "<emptyset>"
   "<nabla>" "<surd>" "<top>" "<bot>" "<angle>"
   "<bflat>" "<natural>" "<sharp>" "<backslash>"
   "<partial>" "<infty>" "<infty>" "<Box>" "<Diamont>"
   "<triangle>" "<clubsuit>" "<diamondsuit>" "<heartsuit>"
   "<spadesuit>" "<diamond>"

   "<backepsilon>" "<backprime>" "<barwedge>" "<because>"
   "<beth>" "<between>" "<bigstar>" "<blacklozenge>"
   "<blacksquare>" "<blacktriangle>" "<blacktriangledown>"
   "<centerdot>" "<checkmark>" "<circledast>" "<circledcirc>"
   "<circleddash>" "<complement>" "<daleth>" "<digamma>"
   "<divideontimes>" "<doublebarwedge>" "<gimel>"
   "<hbar>" "<hslash>" "<intercal>" "<leftthreetimes>" "<llcorner>"
   "<lozenge>" "<lrcorner>" "<maltese>" "<measuredangle>"
   "<pitchfork>" "<rightthreetimes>"
   "<smallfrown>" "<smallsmile>" "<sphericalangle>"
   "<square>" "<therefore>" "<thorn>" "<triangledown>"
   "<triangleq>" "<ulcorner>" "<urcorner>" "<varkappa>"
   "<varnothing>" "<vartriangle>" "<veebar>" "<yen>"

   "<comma>"))
