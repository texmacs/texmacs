
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
  (:synopsis "default semantics for mathematical symbols")

  (define Assign-symbol
    (:type infix)
    (:penalty 3)
    (:spacing wide wide)
    "<assign>" "<backassign>"
    "<plusassign>" "<minusassign>" "<astassign>" "<overassign>")
  
  (define Flux-symbol
    (:type infix)
    (:spacing wide wide)
    "<lflux>" "<gflux>")

  (define Models-symbol
    (:type prefix-infix)
    (:spacing default wide)
    "<models>"
    "<vdash>" "<vDash>" "<Vdash>" "<VDash>" "<Vvdash>" "<VvDash>"
    "<longvdash>" "<longvDash>"
    "<longVdash>" "<longVDash>"
    "<longVvdash>" "<longVvDash>"
    "<nvdash>" "<nvDash>" "<nVdash>" "<nVDash>" "<nVvdash>" "<nVvDash>")

  (define Modeled-symbol
    (:type infix)
    (:spacing default wide)
    "<dashv>" "<Dashv>" "<dashV>" "<DashV>" "<dashVv>" "<DashVv>"
    "<longdashv>" "<longDashv>"
    "<longdashV>" "<longDashV>"
    "<longdashVv>" "<longDashVv>"
    "<ndashv>" "<nDashv>" "<ndashV>" "<nDashV>" "<ndashVv>" "<nDashVv>")

  (define Quantifier-symbol
    (:type prefix)
    "<forall>" "<exists>" "<nexists>" "<Exists>" "<mathlambda>")

  (define Imply-nolim-symbol
    (:type infix)
    (:penalty 10)
    (:spacing wide wide)
    "<implies>" "<equivalent>" "<Leftarrow>" "<Rightarrow>" "<Leftrightarrow>"
    "<Longleftarrow>" "<Longrightarrow>" "<Longleftrightarrow>"
    "<Lleftarrow>" "<Rrightarrow>"
    "<nLeftarrow>" "<nRightarrow>" "<nLeftrightarrow>")

  (define Imply-lim-symbol
    (:type infix)
    (:penalty 10)
    (:spacing wide wide)
    (:limits always)
    "<Leftarrowlim>" "<Rightarrowlim>" "<Leftrightarrowlim>"
    "<Longleftarrowlim>" "<Longrightarrowlim>" "<Longleftrightarrowlim>")
  
  (define Imply-symbol
    Imply-nolim-symbol Imply-symbol)

  (define Or-symbol
    (:type infix)
    (:penalty 10)
    (:spacing wide wide)
    "<vee>" "<curlyvee>" "<veebar>" "<obar>" "<boxbar>")

  (define And-symbol
    (:type infix)
    (:penalty 10)
    (:spacing wide wide)
    "<wedge>" "<curlywedge>" "<barwedge>" "<doublebarwedge>")

  (define Not-symbol
    (:type prefix)
    (:penalty 10)
    "<neg>")

  (define Relation-nolim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing wide wide)

    "=" "<ne>" "<neq>" "<longequal>" "<less>" "<gtr>" "<le>" "<leq>"
    "<prec>" "<preceq>" "<preceqq>" "<ll>" "<lleq>" "<subset>" "<subseteq>"
    "<sqsubset>" "<sqsubseteq>" "<sqsubseteqq>" "<in>" "<ni>" "<of>"
    "<ge>" "<geq>" "<succ>" "<succeq>" "<succeqq>"
    "<gg>" "<ggeq>" "<supset>" "<supseteq>"
    "<sqsupset>" "<sqsupseteq>" "<sqsupseteqq>"
    "<equiv>" "<nequiv>" "<sim>" "<simeq>" "<asymp>" "<approx>" "<cong>"
    "<triplesim>" "<ntriplesim>"
    "<subsetsim>" "<supsetsim>" "<doteq>" "<propto>" "<varpropto>"
    "<perp>" "<bowtie>" "<Join>" "<smile>" "<frown>" "<signchange>"
    "<parallel>" "<shortparallel>" "<nparallel>" "<nshortparallel>"
    "<shortmid>" "<nshortmid>" "<nmid>" "<divides>" "<ndivides>"

    "<approxeq>" "<backsim>" "<backsimeq>"
    "<Bumpeq>" "<bumpeq>" "<circeq>" "<triangleq>"
    "<curlyeqprec>" "<curlyeqsucc>" "<Doteq>" "<doteqdot>" "<eqcirc>"
    "<eqslantgtr>" "<eqslantless>" "<fallingdotseq>" "<geqq>" "<geqslant>"
    "<ggg>" "<gggtr>" "<ngtrapprox>" "<gnapprox>" "<gneq>" "<gneqq>"
    "<ngtrsim>" "<gnsim>" "<gtrapprox>"
    "<gtrdot>" "<gtreqdot>" "<dotgtr>" "<dotgeq>" "<dotgeqslant>"
    "<gtreqless>" "<gtreqqless>" "<gtrless>"
    "<gtrsim>" "<gvertneqq>" "<leqq>" "<leqslant>" "<lessapprox>"
    "<lessdot>" "<lesseqdot>" "<leqdot>" "<leqslantdot>"
    "<lesseqgtr>" "<lesseqqgtr>" "<lessgtr>"
    "<lesssim>" "<lll>" "<llless>" "<nlessapprox>" "<lnapprox>"
    "<lneq>" "<lneqq>" "<nlesssim>" "<lnsim>"
    "<lvertneqq>" "<napprox>" "<ngeq>" "<ngeqq>" "<ngeqslant>"
    "<ngtr>" "<nleq>" "<nleqq>" "<nleqslant>" "<nless>"
    "<nprec>" "<npreceq>" "<precneq>" "<npreceqq>"
    "<nsim>" "<nsimeq>""<ncong>" "<napproxeq>"
    "<nasymp>" "<nsubset>" "<nsupset>" "<nsqsubset>" "<nsqsupset>"
    "<nsqsubseteq>" "<sqsubsetneq>" "<varsqsubsetneq>"
    "<nsqsupseteq>" "<sqsupsetneq>" "<varsqsupsetneq>"
    "<nsqsubseteqq>" "<sqsubsetneqq>" "<varsqsubsetneqq>"
    "<nsqsupseteqq>" "<sqsupsetneqq>" "<varsqsupsetneqq>"
    "<nsubseteq>" "<nsucc>" "<nsucceq>" "<succneq>" "<nsucceqq>"
    "<nsupseteq>" "<nsupseteqq>" "<precapprox>" "<preccurlyeq>"
    "<npreccurlyeq>" "<nprecapprox>" "<precnapprox>"
    "<precneqq>" "<precvertneqq>"
    "<precsim>" "<nprecsim>" "<precnsim>" "<risingdoteq>" "<Subset>"
    "<subseteqq>" "<subsetneq>"
    "<nsubseteqq>" "<subsetneqq>" "<succapprox>"
    "<succcurlyeq>" "<nsucccurlyeq>" "<nsuccapprox>" "<succnapprox>"
    "<succneqq>" "<succvertneqq>"
    "<succsim>" "<nsuccsim>" "<succnsim>" "<Supset>" "<supseteqq>"
    "<nsupsetneqq>" "<supsetneq>" "<supsetneqq>"
    "<thickapprox>" "<thicksim>" "<varsubsetneq>" "<varsubsetneqq>"
    "<varsupsetneq>" "<varsupsetneqq>" "<llleq>" "<gggeq>"
    "<subsetplus>" "<supsetplus>"
    "<nll>" "<nlleq>" "<nlll>" "<nllleq>"
    "<ngg>" "<nggeq>" "<nggg>" "<ngggeq>"

    "<vartriangleleft>" "<vartriangleright>"
    "<triangleleft>" "<triangleright>"
    "<trianglelefteq>" "<trianglerighteq>"
    "<trianglelefteqslant>" "<trianglerighteqslant>"
    "<blacktriangleleft>" "<blacktriangleright>"
    "<blacktrianglelefteq>" "<blacktrianglerighteq>"
    "<blacktrianglelefteqslant>" "<blacktrianglerighteqslant>"
    "<ntriangleleft>" "<ntriangleright>"
    "<ntrianglelefteq>" "<ntrianglerighteq>"
    "<ntrianglelefteqslant>" "<ntrianglerighteqslant>"
    "<nblacktriangleleft>" "<nblacktriangleright>"
    "<nblacktrianglelefteq>" "<nblacktrianglerighteq>"
    "<nblacktrianglelefteqslant>" "<nblacktrianglerighteqslant>"

    "<precprec>" "<precpreceq>" "<precprecprec>" "<precprecpreceq>"
    "<succsucc>" "<succsucceq>" "<succsuccsucc>" "<succsuccsucceq>"
    "<nprecprec>" "<nprecpreceq>" "<nprecprecprec>" "<nprecprecpreceq>"
    "<nsuccsucc>" "<nsuccsucceq>" "<nsuccsuccsucc>" "<nsuccsuccsucceq>"
    "<asympasymp>" "<nasympasymp>" "<simsim>" "<nsimsim>" "<nin>" "<nni>"
    "<notin>" "<notni>"
    "<precdot>" "<preceqdot>" "<preccurlyeqdot>"
    "<dotsucc>" "<dotsucceq>" "<dotsucccurlyeq>"

    "<because>" "<between>" "<therefore>" "<lebar>" "<gebar>"
    "<leangle>" "<geangle>" "<leqangle>" "<geqangle>")

  (define Relation-lim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing wide wide)
    (:limits always)
    "<equallim>" "<longequallim>")

  (define Relation-symbol
    Relation-nolim-symbol Relation-lim-symbol)

  (define Arrow-nolim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)

    "<to>" "<into>" "<from>" "<transtype>" "<leftarrow>" "<rightarrow>"
    "<leftrightarrow>" "<mapsto>" "<mapsfrom>"
    "<hookleftarrow>" "<hookrightarrow>"
    "<longleftarrow>" "<longrightarrow>" "<longleftrightarrow>"
    "<longmapsto>" "<longmapsfrom>"
    "<longhookleftarrow>" "<longhookrightarrow>" "<leftharpoonup>"
    "<leftharpoondown>" "<rightleftharpoons>" "<rightharpoonup>"
    "<rightharpoondown>" "<longleftharpoonup>" "<longleftharpoondown>"
    "<longrightharpoonup>" "<longrightharpoondown>" "<leadsto>" "<nleadsto>"
    "<nearrow>" "<searrow>" "<swarrow>" "<nwarrow>"
    "<longtwoheadleftarrow>" "<longtwoheadrightarrow>"
    "<leftprec>" "<leftpreceq>" "<succright>" "<succeqright>"

    "<circlearrowleft>" "<circlearrowdown>"
    "<curvearrowleft>" "<curvearrowright>"
    "<downdownarrows>" "<downharpoonleft>" "<downharpoonright>"
    "<longdownharpoonleft>" "<longdownharpoonright>"
    "<leftarrowtail>" "<leftleftarrows>" "<leftrightarrows>"
    "<longleftleftarrows>" "<longleftrightarrows>"
    "<leftrightharpoons>" "<rightleftharpoons>"
    "<longleftrightharpoons>" "<longrightleftharpoons>"
    "<looparrowleft>" "<looparrowright>" "<Lsh>"
    "<multimap>" "<mapmulti>" "<longmultimap>" "<longmapmulti>"
    "<nleftarrow>" "<nleftrightarrow>" "<nrightarrow>" "<restriction>"
    "<rightarrowtail>" "<rightleftarrows>" "<rightrightarrows>"
    "<longrightleftarrows>" "<longrightrightarrows>"
    "<leftsquigarrow>" "<rightsquigarrow>" "<leftrightsquigarrow>"
    "<Rsh>" "<twoheadleftarrow>" "<twoheadrightarrow>"
    "<upharpoonleft>" "<upharpoonright>"
    "<longupharpoonleft>" "<longupharpoonright>"
    "<upuparrows>" "<leftrightmap>" "<pointer>")
  
  (define Arrow-lim-symbol
    (:type infix)
    (:penalty 20)
    (:spacing default default)
    (:limits always)
    "<leftarrowlim>" "<rightarrowlim>" "<leftrightarrowlim>"
    "<longleftarrowlim>" "<longrightarrowlim>" "<longleftrightarrowlim>"
    "<mapstolim>" "<longmapstolim>"
    "<leftsquigarrowlim>" "<rightsquigarrowlim>" "<leftrightsquigarrowlim>")

  (define Arrow-symbol
    Arrow-nolim-symbol Arrow-lim-symbol)

  (define Union-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<cup>" "<Cup>" "<doublecup>" "<uplus>" "<sqcup>")

  (define Intersection-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<cap>" "<Cap>" "<doublecap>" "<sqcap>")

  (define Exclude-symbol
    (:type infix)
    (:penalty 30)
    (:spacing default default)
    "<setminus>" "<smallsetminus>" "<bslash>")

  (define Plus-visible-symbol
    (:type prefix-infix)
    (:penalty 30)
    (:spacing default default)
    "+" "<amalg>" "<oplus>" "<boxplus>"
    "<dotplus>" "<dotamalg>" "<dotoplus>" "<pplus>")

  (define Plus-invisible-symbol
    (:type prefix-infix)
    (:penalty invalid)
    (:spacing none default)
    "<noplus>")

  (define Plus-symbol
    Plus-visible-symbol Plus-invisible-symbol)

  (define Plus-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "<upl>")

  (define Minus-symbol
    (:type prefix-infix)
    (:penalty 30)
    (:spacing default default)
    "-" "<pm>" "<mp>" "<minus>" "<longminus>"
    "<dotminus>" "<dotpm>" "<dotmp>"
    "<ominus>" "<boxminus>" "<circleddash>")

  (define Minus-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "<um>" "<upm>" "<ump>")

  (define Times-visible-symbol
    (:type infix)
    (:penalty 40)
    (:spacing default default)
    "<cdot>" "<times>" "<otimes>" "<product>"
    "<circ>" "<bullet>" "<odot>" "<boxdot>" "<boxtimes>" "<boxcircle>"
    "<dottimes>" "<dototimes>"
    "<ltimes>" "<rtimes>" "<btimes>" "<ttimes>"
    "<exterior>" "<join>"
    "<ast>" "<star>" "<oast>" "<boxast>" "<dotast>"
    "<circledast>" "<circledcirc>" "<varocircle>" "<boxbox>"
    "<leftthreetimes>" "<rightthreetimes>"
    "<lefttimesthree>" "<righttimesthree>")

  (define Times-invisible-symbol
    (:type infix)
    (:penalty invalid)
    (:spacing multiply multiply)
    "*")

  (define Times-symbol
    Times-visible-symbol Times-invisible-symbol)

  (define Over-regular-symbol
    (:type infix)
    (:penalty 40)
    (:spacing default default)
    "<over>" "<div>" "<oover>" "<divideontimes>"
    "<oslash>" "<boxslash>" "<dslash>")

  (define Over-condensed-symbol
    (:type infix)
    (:penalty 40)
    (:spacing half half)
    "/")

  (define Over-symbol
    Over-regular-symbol Over-condensed-symbol)

  (define Power-symbol
    (:type infix)
    (:penalty 50)
    "^")

  (define Index-symbol
    (:type infix)
    (:penalty 50)
    "_")

  (define Big-separator-symbol
    "parallel" "interleave")

  (define Big-or-symbol
    "vee" "curlyvee")

  (define Big-and-symbol
    "wedge" "curlywedge")

  (define Big-union-symbol
    "cup" "sqcup" "amalg" "uplus" "pluscup" "box")

  (define Big-intersection-symbol
    "cap" "sqcap")

  (define Big-sum-symbol
    "sum" "oplus" "triangledown"
    ;; NOTE: declaration order is important, because of packrat parsing
    "intlim" "iintlim" "iiintlim" "iiiintlim" "idotsintlim"
    "ointlim" "oiintlim" "oiiintlim"
    "int" "iint" "iiint" "iiiint" "idotsint" "oint" "oiint" "oiiint"
    "upintlim" "upiintlim" "upiiintlim" "upointlim" "upoiintlim" "upoiiintlim"
    "upint" "upiint" "upiiint" "upoint" "upoiint" "upoiiint")

  (define Big-product-symbol
    "prod" "otimes" "odot" "triangleup")

  (define Big-operator-symbol
    Big-separator-symbol
    Big-or-symbol Big-and-symbol
    Big-union-symbol Big-intersection-symbol
    Big-sum-symbol Big-product-symbol)

  (define Big-nolim-symbol
    (:type prefix)
    (:penalty panic)
    (:spacing none big)
    "<big-int>" "<big-iint>" "<big-iiint>" "<big-iiiint>" "<big-idotsint>"
    "<big-oint>" "<big-oiint>" "<big-oiiint>")

  (define Big-lim-symbol
    (:type prefix)
    (:penalty panic)
    (:spacing none big)
    (:limits display)
    "<big-sum>" "<big-prod>" "<big-amalg>"
    "<big-intlim>" "<big-iintlim>" "<big-iiintlim>"
    "<big-iiiintlim>" "<big-idotsintlim>"
    "<big-ointlim>" "<big-oiintlim>" "<big-oiiintlim>"
    "<big-cap>" "<big-cup>" "<big-sqcap>" "<big-sqcup>"
    "<big-vee>" "<big-wedge>" "<big-curlyvee>" "<big-curlywedge>"
    "<big-odot>" "<big-otimes>" "<big-oplus>" "<big-uplus>"
    "<big-triangleup>" "<big-triangledown>"
    "<big-box>" "<big-parallel>" "<big-interleave>")

  (define Big-symbol
    Big-nolim-symbol Big-lim-symbol)

  (define Other-prefix-symbol
    (:type prefix)
    (:penalty invalid)
    (:spacing none none)
    "#" "<card>")

  (define Other-postfix-symbol
    (:type postfix)
    (:penalty panic)
    "!" "%" "<permil>"
    "<postup>" "<postdown>" "<postupdown>" "<postmapsup>" "<postmapsdown>")

  (define Prime-symbol
    (:type symbol)
    (:penalty panic)
    "'" "`" "<dag>" "<ddag>" "<asterisk>" "<star>" "<kreuz>")

  (define Ponctuation-visible-symbol
    (:type separator)
    (:penalty 0)
    (:spacing none default)
    "," ";" ":" "<point>" "<mid>")

  (define Ponctuation-invisible-symbol
    (:type separator)
    (:penalty invalid)
    (:spacing none none)
    "<nocomma>")

  (define Ponctuation-symbol
    Ponctuation-visible-symbol Ponctuation-invisible-symbol)

  (define Open-symbol
    (:type opening-bracket)
    "(" "[" "{"
    "<lvert>" "<lVert>"
    "<lfloor>" "<lceil>" "<langle>"
    "<llbracket>" "<llangle>"
    "<left-(>" "<left-[>" "<left-{>" "<left-less>"
    "<left-}>" "<left-]>" "<left-)>" "<left-gtr>"
    "<left-|>" "<left-||>" "<left-.>"
    "<left-lfloor>" "<left-lceil>" "<left-rfloor>" "<left-rceil>"
    "<left-langle>" "<left-rangle>")
  
  (define Middle-bracket-symbol
    (:type middle-bracket)
    (:spacing middle middle)
    "|" "<||>" "<nobracket>" "<mid-|>" "<mid-||>" "<mid-.>")
  
  (define Middle-separator-symbol
    (:type middle-bracket)
    (:penalty 0)
    (:spacing default default)
    "<suchthat>" "<barsuchthat>")

  (define Middle-symbol
    Middle-bracket-symbol Middle-separator-symbol)

  (define Close-symbol
    (:type closing-bracket)
    "}" "]" ")"
    "<rvert>" "<rVert>"
    "<rfloor>" "<rceil>" "<rangle>"
    "<rrbracket>" "<rrangle>"
    "<right-(>" "<right-[>" "<right-{>" "<right-less>"
    "<right-}>" "<right-]>" "<right-)>" "<right-gtr>"
    "<right-|>" "<right-||>" "<right-.>"
    "<right-lfloor>" "<right-lceil>" "<right-rfloor>" "<right-rceil>"
    "<right-langle>" "<right-rangle>")

  (define Suspension-nolim-symbol
    (:type symbol)
    (:penalty invalid invalid)
    "<ldots>" "<cdots>" "<hdots>" "<vdots>"
    "<ddots>" "<udots>" "<mdots>" "<colons>")
  
  (define Suspension-lim-symbol
    (:type symbol)
    (:penalty invalid invalid)
    (:limits always)
    "<cdotslim>")

  (define Suspension-symbol
    Suspension-nolim-symbol Suspension-lim-symbol)

  (define Letter-symbol
    (:type symbol)

    "<mathe>" "<mathi>" "<mathlambda>" "<mathpi>"
    "<matheuler>" "<mathcatalan>" "<mathGamma>"
    "<aleph>" "<beth>" "<gimel>" "<daleth>"
    "<hbar>" "<hslash>" "<wp>" "<digamma>"
    "<imath>" "<b-imath>" "<jmath>" "<b-jmath>" "<ell>" "<b-ell>"
    "<cal-imath>" "<b-cal-imath>" "<cal-jmath>" "<b-cal-jmath>"
    "<frak-imath>" "<frak-jmath>" "<bbb-imath>" "<bbb-jmath>"

    "<alpha>" "<beta>" "<gamma>" "<delta>" "<epsilon>"
    "<varepsilon>" "<zeta>" "<eta>" "<theta>" "<vartheta>"
    "<iota>" "<kappa>" "<varkappa>" "<lambda>" "<mu>" "<nu>"
    "<xi>" "<omicron>" "<pi>" "<varpi>" "<rho>" "<varrho>"
    "<sigma>" "<varsigma>" "<tau>" "<upsilon>"
    "<phi>" "<varphi>" "<psi>" "<chi>" "<omega>"
    "<backepsilon>" "<mho>"

    "<Alpha>" "<Beta>" "<Gamma>" "<Delta>" "<Epsilon>" "<Zeta>"
    "<Eta>" "<Theta>" "<Iota>" "<Kappa>" "<Lambda>" "<Mu>" "<Nu>"
    "<Xi>" "<Omicron>" "<Pi>" "<Rho>" "<Sigma>" "<Tau>"
    "<Upsilon>" "<Phi>" "<Psi>" "<Chi>" "<Omega>"
    "<Backepsilon>" "<Backsigma>" "<Mho>"

    "<b-0>" "<b-1>" "<b-2>" "<b-3>" "<b-4>"
    "<b-5>" "<b-6>" "<b-7>" "<b-8>" "<b-9>"

    "<b-a>" "<b-b>" "<b-c>" "<b-d>" "<b-e>" "<b-f>" "<b-g>"
    "<b-h>" "<b-i>" "<b-j>" "<b-k>" "<b-l>" "<b-m>" "<b-n>"
    "<b-o>" "<b-p>" "<b-q>" "<b-r>" "<b-s>" "<b-t>" "<b-u>"
    "<b-v>" "<b-w>" "<b-x>" "<b-y>" "<b-z>"

    "<b-A>" "<b-B>" "<b-C>" "<b-D>" "<b-E>" "<b-F>" "<b-G>"
    "<b-H>" "<b-I>" "<b-J>" "<b-K>" "<b-L>" "<b-M>" "<b-N>"
    "<b-O>" "<b-P>" "<b-Q>" "<b-R>" "<b-S>" "<b-T>" "<b-U>"
    "<b-V>" "<b-W>" "<b-X>" "<b-Y>" "<b-Z>"

    "<b-alpha>" "<b-beta>" "<b-gamma>" "<b-delta>" "<b-epsilon>"
    "<b-varepsilon>" "<b-zeta>" "<b-eta>" "<b-theta>" "<b-vartheta>"
    "<b-iota>" "<b-kappa>" "<b-varkappa>" "<b-lambda>" "<b-mu>" "<b-nu>"
    "<b-xi>" "<b-omicron>" "<b-pi>" "<b-varpi>" "<b-rho>" "<b-varrho>"
    "<b-sigma>" "<b-varsigma>" "<b-tau>" "<b-upsilon>"
    "<b-phi>" "<b-varphi>" "<b-psi>" "<b-chi>" "<b-omega>"
    "<b-backepsilon>" "<b-mho>"

    "<b-Alpha>" "<b-Beta>" "<b-Gamma>" "<b-Delta>" "<b-Epsilon>"
    "<b-Zeta>" "<b-Eta>" "<b-Theta>" "<b-Iota>" "<b-Kappa>"
    "<b-Lambda>" "<b-Mu>" "<b-Nu>" "<b-Xi>" "<b-Omicron>"
    "<b-Pi>" "<b-Rho>" "<b-Sigma>" "<b-Tau>" "<b-Upsilon>"
    "<b-Phi>" "<b-Psi>" "<b-Chi>" "<b-Omega>"
    "<b-Backepsilon>" "<b-Backsigma>" "<b-Mho>"

    "<up-a>" "<up-b>" "<up-c>" "<up-d>" "<up-e>" "<up-f>" "<up-g>"
    "<up-h>" "<up-i>" "<up-j>" "<up-k>" "<up-l>" "<up-m>" "<up-n>"
    "<up-o>" "<up-p>" "<up-q>" "<up-r>" "<up-s>" "<up-t>" "<up-u>"
    "<up-v>" "<up-w>" "<up-x>" "<up-y>" "<up-z>"
    "<up-imath>" "<up-jmath>" "<up-ell>"

    "<up-A>" "<up-B>" "<up-C>" "<up-D>" "<up-E>" "<up-F>" "<up-G>"
    "<up-H>" "<up-I>" "<up-J>" "<up-K>" "<up-L>" "<up-M>" "<up-N>"
    "<up-O>" "<up-P>" "<up-Q>" "<up-R>" "<up-S>" "<up-T>" "<up-U>"
    "<up-V>" "<up-W>" "<up-X>" "<up-Y>" "<up-Z>"

    "<up-alpha>" "<up-beta>" "<up-gamma>" "<up-delta>" "<up-epsilon>"
    "<up-varepsilon>" "<up-zeta>" "<up-eta>" "<up-theta>" "<up-vartheta>"
    "<up-iota>" "<up-kappa>" "<up-varkappa>" "<up-lambda>" "<up-mu>"
    "<up-nu>" "<up-xi>" "<up-omicron>" "<up-pi>" "<up-varpi>" "<up-rho>"
    "<up-varrho>" "<up-sigma>" "<up-varsigma>" "<up-tau>" "<up-upsilon>"
    "<up-phi>" "<up-varphi>" "<up-psi>" "<up-chi>" "<up-omega>"
    "<up-backepsilon>" "<up-mho>"

    "<up-Alpha>" "<up-Beta>" "<up-Gamma>" "<up-Delta>" "<up-Epsilon>"
    "<up-Zeta>" "<up-Eta>" "<up-Theta>" "<up-Iota>" "<up-Kappa>"
    "<up-Lambda>" "<up-Mu>" "<up-Nu>" "<up-Xi>" "<up-Omicron>"
    "<up-Pi>" "<up-Rho>" "<up-Sigma>" "<up-Tau>" "<up-Upsilon>"
    "<up-Phi>" "<up-Psi>" "<up-Chi>" "<up-Omega>"
    "<up-Backepsilon>" "<up-Mho>"

    "<b-up-a>" "<b-up-b>" "<b-up-c>" "<b-up-d>" "<b-up-e>" "<b-up-f>"
    "<b-up-g>" "<b-up-h>" "<b-up-i>" "<b-up-j>" "<b-up-k>" "<b-up-l>"
    "<b-up-m>" "<b-up-n>" "<b-up-o>" "<b-up-p>" "<b-up-q>" "<b-up-r>"
    "<b-up-s>" "<b-up-t>" "<b-up-u>" "<b-up-v>" "<b-up-w>" "<b-up-x>"
    "<b-up-y>" "<b-up-z>"

    "<b-up-A>" "<b-up-B>" "<b-up-C>" "<b-up-D>" "<b-up-E>" "<b-up-F>"
    "<b-up-G>" "<b-up-H>" "<b-up-I>" "<b-up-J>" "<b-up-K>" "<b-up-L>"
    "<b-up-M>" "<b-up-N>" "<b-up-O>" "<b-up-P>" "<b-up-Q>" "<b-up-R>"
    "<b-up-S>" "<b-up-T>" "<b-up-U>" "<b-up-V>" "<b-up-W>" "<b-up-X>"
    "<b-up-Y>" "<b-up-Z>"

    "<b-up-alpha>" "<b-up-beta>" "<b-up-gamma>" "<b-up-delta>"
    "<b-up-epsilon>" "<b-up-varepsilon>" "<b-up-zeta>" "<b-up-eta>"
    "<b-up-theta>" "<b-up-vartheta>" "<b-up-iota>" "<b-up-kappa>"
    "<b-up-varkappa>" "<b-up-lambda>" "<b-up-mu>" "<b-up-nu>"
    "<b-up-xi>" "<b-up-omicron>" "<b-up-pi>" "<b-up-varpi>" "<b-up-rho>"
    "<b-up-varrho>" "<b-up-sigma>" "<b-up-varsigma>" "<b-up-tau>"
    "<b-up-upsilon>" "<b-up-phi>" "<b-up-varphi>" "<b-up-psi>"
    "<b-up-chi>" "<b-up-omega>" "<b-up-backepsilon>" "<b-up-mho>"

    "<b-up-Alpha>" "<b-up-Beta>" "<b-up-Gamma>" "<b-up-Delta>"
    "<b-up-Epsilon>" "<b-up-Zeta>" "<b-up-Eta>" "<b-up-Theta>"
    "<b-up-Iota>" "<b-up-Kappa>" "<b-up-Lambda>" "<b-up-Mu>"
    "<b-up-Nu>" "<b-up-Xi>" "<b-up-Omicron>" "<b-up-Pi>"
    "<b-up-Rho>" "<b-up-Sigma>" "<b-up-Tau>" "<b-up-Upsilon>"
    "<b-up-Phi>" "<b-up-Psi>" "<b-up-Chi>" "<b-up-Omega>"
    "<b-up-Backepsilon>" "<b-up-Mho>"
    
    "<cal-a>" "<cal-b>" "<cal-c>" "<cal-d>" "<cal-e>" "<cal-f>" "<cal-g>"
    "<cal-h>" "<cal-i>" "<cal-j>" "<cal-k>" "<cal-l>" "<cal-m>" "<cal-n>"
    "<cal-o>" "<cal-p>" "<cal-q>" "<cal-r>" "<cal-s>" "<cal-t>" "<cal-u>"
    "<cal-v>" "<cal-w>" "<cal-x>" "<cal-y>" "<cal-z>"
    
    "<cal-A>" "<cal-B>" "<cal-C>" "<cal-D>" "<cal-E>" "<cal-F>" "<cal-G>"
    "<cal-H>" "<cal-I>" "<cal-J>" "<cal-K>" "<cal-L>" "<cal-M>" "<cal-N>"
    "<cal-O>" "<cal-P>" "<cal-Q>" "<cal-R>" "<cal-S>" "<cal-T>" "<cal-U>"
    "<cal-V>" "<cal-W>" "<cal-X>" "<cal-Y>" "<cal-Z>"
    
    "<b-cal-a>" "<b-cal-b>" "<b-cal-c>" "<b-cal-d>" "<b-cal-e>" "<b-cal-f>"
    "<b-cal-g>" "<b-cal-h>" "<b-cal-i>" "<b-cal-j>" "<b-cal-k>" "<b-cal-l>"
    "<b-cal-m>" "<b-cal-n>" "<b-cal-o>" "<b-cal-p>" "<b-cal-q>" "<b-cal-r>"
    "<b-cal-s>" "<b-cal-t>" "<b-cal-u>" "<b-cal-v>" "<b-cal-w>" "<b-cal-x>"
    "<b-cal-y>" "<b-cal-z>"

    "<b-cal-A>" "<b-cal-B>" "<b-cal-C>" "<b-cal-D>" "<b-cal-E>" "<b-cal-F>"
    "<b-cal-G>" "<b-cal-H>" "<b-cal-I>" "<b-cal-J>" "<b-cal-K>" "<b-cal-L>"
    "<b-cal-M>" "<b-cal-N>" "<b-cal-O>" "<b-cal-P>" "<b-cal-Q>" "<b-cal-R>"
    "<b-cal-S>" "<b-cal-T>" "<b-cal-U>" "<b-cal-V>" "<b-cal-W>" "<b-cal-X>"
    "<b-cal-Y>" "<b-cal-Z>"

    "<frak-a>" "<frak-b>" "<frak-c>" "<frak-d>" "<frak-e>" "<frak-f>"
    "<frak-g>" "<frak-h>" "<frak-i>" "<frak-j>" "<frak-k>" "<frak-l>"
    "<frak-m>" "<frak-n>" "<frak-o>" "<frak-p>" "<frak-q>" "<frak-r>"
    "<frak-s>" "<frak-t>" "<frak-u>" "<frak-v>" "<frak-w>" "<frak-x>"
    "<frak-y>" "<frak-z>"

    "<frak-A>" "<frak-B>" "<frak-C>" "<frak-D>" "<frak-E>" "<frak-F>"
    "<frak-G>" "<frak-H>" "<frak-I>" "<frak-J>" "<frak-K>" "<frak-L>"
    "<frak-M>" "<frak-N>" "<frak-O>" "<frak-P>" "<frak-Q>" "<frak-R>"
    "<frak-S>" "<frak-T>" "<frak-U>" "<frak-V>" "<frak-W>" "<frak-X>"
    "<frak-Y>" "<frak-Z>"

    "<bbb-a>" "<bbb-b>" "<bbb-c>" "<bbb-d>" "<bbb-e>" "<bbb-f>" "<bbb-g>"
    "<bbb-h>" "<bbb-i>" "<bbb-j>" "<bbb-k>" "<bbb-l>" "<bbb-m>" "<bbb-n>"
    "<bbb-o>" "<bbb-p>" "<bbb-q>" "<bbb-r>" "<bbb-s>" "<bbb-t>" "<bbb-u>"
    "<bbb-v>" "<bbb-w>" "<bbb-x>" "<bbb-y>" "<bbb-z>"

    "<bbb-A>" "<bbb-B>" "<bbb-C>" "<bbb-D>" "<bbb-E>" "<bbb-F>" "<bbb-G>"
    "<bbb-H>" "<bbb-I>" "<bbb-J>" "<bbb-K>" "<bbb-L>" "<bbb-M>" "<bbb-N>"
    "<bbb-O>" "<bbb-P>" "<bbb-Q>" "<bbb-R>" "<bbb-S>" "<bbb-T>" "<bbb-U>"
    "<bbb-V>" "<bbb-W>" "<bbb-X>" "<bbb-Y>" "<bbb-Z>"

    "<bbb-0>" "<bbb-1>" "<bbb-2>" "<bbb-3>" "<bbb-4>"
    "<bbb-5>" "<bbb-6>" "<bbb-7>" "<bbb-8>" "<bbb-9>")
  
  (define Miscellaneous-symbol
    (:type symbol)

    "<ldot>" "<udot>" "<comma>" "<cdummy>" "<nosymbol>"

    "<uparrow>" "<Uparrow>" "<downarrow>" "<Downarrow>"
    "<updownarrow>" "<Updownarrow>" "<mapsup>" "<mapsdown>"
    "<hookuparrow>" "<hookdownarrow>"
    "<upsquigarrow>" "<downsquigarrow>" "<updownsquigarrow>"
    "<upminus>" "<downminus>" "<upequal>" "<downequal>"
    "<longuparrow>" "<Longuparrow>"
    "<longdownarrow>" "<Longdownarrow>" "<longupdownarrow>" "<Longupdownarrow>"
    "<longmapsup>" "<longmapsdown>" "<longhookuparrow>" "<longhookdownarrow>"
    "<longupequal>" "<longdownequal>" "<longupminus>" "<longdownminus>"

    "<prime>" "<emptyset>"
    "<surd>" "<top>" "<bot>" "<angle>"
    "<flat>" "<natural>" "<sharp>" "<backslash>"
    "<infty>" "<infty>" "<Box>" "<Diamont>" "<oempty>" "<boxempty>"
    "<triangle>" "<clubsuit>" "<diamondsuit>" "<heartsuit>"
    "<spadesuit>" "<diamond>" "<box>" "<smallbox>" "<bullet>"
    "<eigthnote>" "<quarternote>" "<halfnote>" "<fullnote>" "<twonotes>"
    "<sun>" "<leftmoon>" "<rightmoon>" "<earth>" "<male>" "<female>"
    "<kreuz>" "<recorder>" "<phone>" "<checked>" "<bell>"

    "<backprime>" "<bigstar>" "<blacklozenge>"
    "<blacksquare>" "<blacktriangle>"
    "<blacktriangleup>" "<blacktriangledown>"
    "<centerdot>" "<checkmark>"
    "<intercal>" "<llcorner>"
    "<lozenge>" "<lrcorner>" "<maltese>" "<measuredangle>"
    "<pitchfork>" "<smallfrown>" "<smallsmile>" "<sphericalangle>"
    "<square>" "<thorn>" "<triangledown>"
    "<ulcorner>" "<urcorner>" "<varkappa>"
    "<varnothing>" "<vartriangle>" "<yen>")

  (define Spacing-visible-symbol
    (:type infix)
    (:spacing half none)
    " ")

  (define Spacing-wide-symbol
    (:type infix)
    (:spacing big none)
    "<space>")

  (define Spacing-invisible-symbol
    (:type infix)
    (:penalty invalid)
    (:spacing none none)
    "<nospace>")

  (define Spacing-symbol
    Spacing-visible-symbol Spacing-wide-symbol Spacing-invisible-symbol)

  (define Unary-operator-glyph-symbol
    (:type unary)
    (:penalty invalid)
    (:spacing none none)
    "<mathd>" "<mathD>" "<mathLaplace>" "<partial>" "<nabla>"
    "<Re>" "<Im>" "<complement>"
    "<sum>" "<prod>"
    "<int>" "<iint>" "<iiint>" "<iiiint>" "<idotsint>"
    "<oint>" "<oiint>" "<oiiint>"
    "<intlim>" "<iintlim>" "<iiintlim>" "<iiiintlim>" "<idotsintlim>"
    "<ointlim>" "<oiintlim>" "<oiiintlim>")

  ;; FIXME: spacing behind $\sin$ is currently incorrect,
  ;; because the transition OP_UNARY -> OP_TEXT is not detected
  ;; in concater_rep::typeset_math_string.  The unary operators
  ;; can be uncommented as soon as this problem will be corrected.
  ;;
  ;;(define Unary-operator-textual-symbol
  ;;  (:type unary)
  ;;  (:penalty invalid)
  ;;  (:spacing none default)
  ;;  "arccos" "arcsin" "arctan" "cos" "cosh" "cot" "coth" "csc"
  ;;  "deg" "det" "dim" "exp" "hom" "ker" "lg" "ln" "log"
  ;;  "Pr" "sec" "sin" "sinh" "tan" "tanh")

  (define Unary-operator-symbol
    Unary-operator-glyph-symbol
    Unary-operator-textual-symbol)

  (define Binary-operator-symbol
    (:type binary)
    (:penalty invalid)
    (:spacing none default)
    "div" "mod")

  (define N-ary-operator-symbol
    (:type n-ary)
    (:penalty invalid)
    (:spacing none default)
    (:limits display)
    "gcd" "gcrd" "inf" "lclm" "lcm" "lim"
    "liminf" "limsup" "max" "min" "statlim" "sup")

  (define Prefix-symbol
    Not-symbol
    (:<big Big-operator-symbol :>)
    Minus-prefix-symbol
    Plus-prefix-symbol
    Other-prefix-symbol)

  (define Postfix-symbol
    Other-postfix-symbol)

  (define Infix-symbol
    Assign-symbol
    Models-symbol
    Modeled-symbol
    Imply-symbol
    Or-symbol
    And-symbol
    Relation-symbol
    Arrow-symbol
    Union-symbol
    Intersection-symbol
    Exclude-symbol
    Plus-symbol
    Minus-symbol
    Times-symbol
    Over-symbol
    Power-symbol)

  (define Reserved-symbol
    :<frac :<sqrt :<wide :<table :<row
    :<around :<around* :<big-around :<left :<mid :<right :<big
    :<lsub :<lsup :<rsub :<rsup :<lprime :<rprime
    :<Prefix :<Postfix)

  (attach-macro
    ("<ldots>" low-dots)
    ("<cdots>" center-dots)
    ("<hdots>" high-dots)
    ("<vdots>" vertical-dots)
    ("<ddots>" diagonal-dots)
    ("<udots>" upward-dots)))
