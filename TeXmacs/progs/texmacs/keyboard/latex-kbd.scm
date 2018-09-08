
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-kbd.scm
;; DESCRIPTION : setup key combinations for frequently used commands
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard latex-kbd)
  (:use (utils edit auto-close)
	(generic format-edit)
	(generic generic-edit)
	(text text-edit)))

(kbd-symbols
  "alpha" "beta" "gamma" "delta" "epsilon"
  "zeta" "eta" "theta" "iota" "kappa"
  "lambda" "mu" "nu" "xi" "omicron" "pi" "rho"
  "sigma" "tau" "upsilon" "phi" "chi"
  "psi" "omega" "varepsilon" "vartheta"
  "varpi" "varrho" "varsigma" "varphi"
  "leftharpoonup" "leftharpoondown"
  "rightharpoonup" "rightharpoondown" "lefthook" "righthook"
  "triangleright" "triangleleft" "star" "partial"
  "flat" "natural" "sharp" "smile" "frown" "ell"
  "imath" "jmath" "wp" "vect"
  
  "Alpha" "Beta" "Gamma" "Delta" "Epsilon"
  "Zeta" "Eta" "Theta" "Iota" "Kappa" "Lambda"
  "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho"
  "Sigma" "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega"
  "grave" "acute" "check" "breve" "invbreve" "bar"
  "cedille"
  "AA" "AE" "DH" "L" "NG" "O" "OE" "S" "SS" "TH"
  "aa" "ae" "dh" "dj" "i" "j" "l" "ng" "o" "oe" "ss" "sz" "th"

  "ast" "asterisk"
  
  "cdot" "times" "asterisk" "div" "diamond"
  "pm" "mp" "oplus" "ominus" "otimes" "oslash"
  "odot" "bigcirc" "circ" "bullet" "asymp" "equiv"
  "subseteq" "supseteq" "leq" "geq" "preceq" "succeq"
  "sim" "approx" "subset" "supset" "ll" "gg"
  "prec" "succ" "leftarrow" "rightarrow" "uparrow"
  "downarrow" "leftrightarrow" "nearrow" "searrow"
  "simeq" "Leftarrow" "Rightarrow" "Uparrow"
  "Downarrow" "Leftrightarrow" "nwarrow" "swarrow"
  "propto" "infty" "in" "ni" "mid" "bigtriangleup" "triangle"
  "bigtriangledown" "negate" "varshortmid" "forall" "exists"
  "neg" "emptyset" "Re" "Im" "top" "bot" "aleph" "perp"
  "cup" "cap" "uplus" "wedge" "vee" "vdash" "dashv"
  "lfloor" "rfloor" "lceil" "rceil" "langle" "rangle"
  "updownarrow" "Updownarrow" "wr" "amalg" "nabla"
  "sqcup" "sqcap" "sqsubseteq" "sqsupseteq" "dag" "ddag"
  "endofline" "clubsuit" "diamondsuit" "heartsuit" "spadesuit"
  "backslash" "setminus"

  "boxdot" "boxplus" "boxtimes" "box" "blacksquare"
  "centerdot" "lozenge" "blacklozenge" "circlearrowright"
  "circlearrowleft" "rightleftharpoons" "leftrightharpoons"
  "boxminus" "Vdash" "Vvdash" "dashV" "twoheadrightarrow"
  "twoheadleftarrow" "leftleftarrows" "rightrightarrows"
  "upuparrows" "downdownarrows" "upharpoonright"
  "downharpoonright" "upharpoonleft" "downharpoonleft"
  "rightarrowtail" "leftarrowtail" "leftrightarrows"
  "rightleftarrows" "Lsh" "Rsh" "rightsquigarrow"
  "leftrightsquigarrow" "looparrowleft" "looparrowright"
  "circeq" "succsim" "gtrsim" "gtrapprox" "multimap"
  "therefore" "because" "doteqdot" "triangleq"
  "precsim" "lesssim" "lessapprox" "eqslantless"
  "eqslantgtr" "curlyeqprec" "curlyeqsucc" "preccurlyeq"
  "leqq" "leqslant" "lessgtr" "smalldash"
  "risingdotseq" "fallingdotseq" "succcurlyeq" "geqq"
  "geqslant" "gtrless" "sqsubset" "sqsupset"
  "vartriangleright" "vartriangleleft" "trianglerighteq"
  "trianglelefteq" "bigstar" "between"
  "blacktriangledown" "blacktriangleright" "blacktriangleleft"
  "shortrightarrow" "shortleftarrow" "vartriangle"
  "blacktriangle" "triangledown" "eqcirc"
  "lesseqgtr" "gtreqless" "lesseqqgtr" "gtreqqless"
  "yen" "Rrightarrow" "Lleftarrow" "checkmark"
  "veebar" "barwedge" "doublebarwedge" "angle" "measuredangle"
  "sphericalangle" "varpropto" "smallsmile" "smallfrown" "Subset"
  "Supset" "Cup" "Cap" "curlyvee" "curlywedge"
  "leftthreetimes" "rightthreetimes" "subseteqq" "supseteqq"
  "bumpeq" "Bumpeq" "lll" "ggg" "ulcorner" "urcorner"
  "circledR" "circledS" "pitchfork" "dotplus" "backsim"
  "backsimeq" "llcorner" "lrcorner" "maltese"
  "complement" "intercal" "circledcirc" "circledast" "circleddash"
  
  "lvertneqq" "gvertneqq" "nleq" "ngeq" "nless" "ngtr"
  "nprec" "nsucc" "lneqq" "gneqq" "nleqslant" "ngeqslant"
  "lneq" "gneq" "npreceq" "nsucceq" "precnsim"
  "succnsim" "lnsim" "gnsim" "nleqq" "ngeqq"
  "precneqq" "succneqq" "precnapprox" "succnapprox"
  "lnapprox" "gnapprox" "nsim" "ncong" "diagup"
  "diagdown" "varsubsetneq" "varsupsetneq"
  "nsubseteqq" "nsupseteqq" "subsetneqq" "supsetneqq"
  "varsubsetneqq" "varsupsetneqq" "subsetneq" "supsetneq"
  "nsubseteq" "nsupseteq" "parallel" "nparallel" "nmid" "nshortmid"
  "nshortparallel" "nvdash" "nVdash" "nvDash" "nVDash" "models"
  "ntrianglerighteq" "ntrianglelefteq" "ntriangleleft"
  "ntriangleright" "nleftarrow" "nrightarrow" "nLeftarrow"
  "nRightarrow" "nLeftrightarrow" "nleftrightarrow"
  "divideontimes" "varnothing" "nexists"
  "Mho" "thorn" "eth" "eqsim" "beth" "gimel" "daleth"
  "lessdot" "gtrdot" "ltimes" "rtimes" "shortmid"
  "shortparallel" "smallsetminus" "thicksim" "thickapprox"
  "approxeq" "succapprox" "precapprox" "curvearrowleft"
  "curvearrowright" "digamma" "varkappa" "hslash"
  "hbar" "backepsilon"
  
  "mapsto" "longmapsto" "longrightarrow" "longleftarrow"
  "longleftrightarrow" "longRightarrow" "longLeftarrow"
  "longLeftrightarrow" "ldots" "cdots" "vdots"
  "ddots" "dotamalg" "dottimes" "dotoplus" "dototimes" "venus"
  "mars" "earth" "aries" "fullmoon" "newmoon" "astrosun"
  "leo" "LEFTcircle" "RIGHTcircle" "Square" "CheckedBox" "photon" "gluon"
  "neq" "notin" "nin" "notni" "nni" "nll" "nlll" "ngg" "nggg"
  "dagger" "ddagger" "boxbar" "checked" "obar" "kreuz")

(kbd-commands
  ("#" "Insert sharp" (insert "#"))
  ("$" "Insert dollar" (insert "$"))
  ("(" "Insert formula" (make-with "mode" "math"))
  ("," "Insert small horizontal space" (make-space "0.2spc"))
  (":" "Insert medium horizontal space" (make-space "0.4spc"))
  (";" "Insert thick horizontal space" (make-space "0.6spc"))
  (" " "Insert horizontal interword space" (make-space "1spc"))
  ("!" "Insert negative horizontal space" (make-space "-0.2spc"))
  ("|" "Insert norm" (insert "<||>"))
  ("quad" "Insert a quad space" (make-space "1em"))
  ("qquad" "Insert a qquad space" (make-space "2em"))
  ("\\" "Go to the next line" (make 'next-line))

  ("[" "Insert equation" (make-equation*))
  ("equation" "Insert numbered equation" (make-equation))
  ("eqnarray*" "Insert equation array" (make-eqnarray*))

  ("cC" "Make Ç" (emulate-keyboard "cedilla C"))
  ("cc" "Make ç" (emulate-keyboard "cedilla c"))
  ("'E" "Insert É" (emulate-keyboard "acute E"))
  ("'e" "Insert é" (emulate-keyboard "acute e"))
  ("`A" "Insert À" (emulate-keyboard "grave A"))
  ("`E" "Insert È" (emulate-keyboard "grave E"))
  ("`U" "Insert Ù" (emulate-keyboard "grave U"))
  ("`a" "Insert à" (emulate-keyboard "grave a"))
  ("`e" "Insert è" (emulate-keyboard "grave e"))
  ("`u" "Insert ù" (emulate-keyboard "grave u"))
  ("^A" "Insert Â" (emulate-keyboard "hat A"))
  ("^E" "Insert Ê" (emulate-keyboard "hat E"))
  ("^I" "Insert Î" (emulate-keyboard "hat I"))
  ("^O" "Insert Ô" (emulate-keyboard "hat O"))
  ("^U" "Insert Û" (emulate-keyboard "hat U"))
  ("^a" "Insert â" (emulate-keyboard "hat a"))
  ("^e" "Insert ê" (emulate-keyboard "hat e"))
  ("^i" "Insert î" (emulate-keyboard "hat i"))
  ("^o" "Insert ô" (emulate-keyboard "hat o"))
  ("^u" "Insert û" (emulate-keyboard "hat u"))
  ("S" "Make Ÿ" (emulate-keyboard "S-F5 p"))
  ("aa" "Insert aa" (emulate-keyboard "abovering a"))
  ("oe" "Insert oe" (emulate-keyboard "S-F5 o e"))
  ("ae" "Insert ae" (emulate-keyboard "S-F5 a"))
  ("ss" "Insert ÿ" (emulate-keyboard "S-F5 s"))
  ("AA" "Insert AA" (emulate-keyboard "abovering A"))
  ("OE" "Insert OE" (emulate-keyboard "S-F5 O E"))
  ("AE" "Insert AE" (emulate-keyboard "S-F5 A"))
  ("SS" "Insert SS" (emulate-keyboard "S-F5 S"))

  ("arccos" "Insert arccos" (insert "arccos"))
  ("arcsin" "Insert arcsin" (insert "arcsin"))
  ("arctan" "Insert arctan" (insert "arctan"))
  ("arg" "Insert arg" (insert "arg"))
  ("cos" "Insert cos" (insert "cos"))
  ("cosh" "Insert cosh" (insert "cosh"))
  ("cot" "Insert cot" (insert "cot"))
  ("coth" "Insert coth" (insert "coth"))
  ("csc" "Insert csc" (insert "csc"))
  ("deg" "Insert deg" (insert "deg"))
  ("det" "Insert det" (insert "det"))
  ("dim" "Insert dim" (insert "dim"))
  ("exp" "Insert exp" (insert "exp"))
  ("gcd" "Insert gcd" (insert "gcd"))
  ("hom" "Insert hom" (insert "hom"))
  ("inf" "Insert inf" (insert "inf"))
  ("ker" "Insert ker" (insert "ker"))
  ("lg" "Insert lg" (insert "lg"))
  ("lim" "Insert lim" (insert "lim"))
  ("liminf" "Insert liminf" (insert "liminf"))
  ("limsup" "Insert limsup" (insert "limsup"))
  ("ln" "Insert ln" (insert "ln"))
  ("log" "Insert log" (insert "log"))
  ("max" "Insert max" (insert "max"))
  ("min" "Insert min" (insert "min"))
  ("Pr" "Insert Pr" (insert "Pr"))
  ("sec" "Insert sec" (insert "sec"))
  ("sin" "Insert sin" (insert "sin"))
  ("sinh" "Insert sinh" (insert "sinh"))
  ("sup" "Insert sup" (insert "sup"))
  ("tan" "Insert tan" (insert "tan"))
  ("tanh" "Insert tanh" (insert "tanh"))

  ("abstract"  "Insert abstract" (make-abstract-data))
  ("maketitle" "Insert title"    (make-doc-data))
  ("tableofcontents" "Insert table of contents"
   (make-aux "table-of-contents" "toc"))
  ("appendix" "Insert appendix" (make-section 'appendix))
  ("chapter" "Insert chapter" (make-section 'chapter))
  ("section" "Insert section" (make-section 'section))
  ("subsection" "Insert subsection" (make-section 'subsection))
  ("subsubsection" "Insert subsubsection" (make-section 'subsubsection))
  ("paragraph" "Insert paragraph" (make-section 'paragraph))
  ("subparagraph" "Insert subparagraph" (make-section 'subparagraph))
  ("table" "Insert a big table" (make 'big-table))
  ("figure" "Insert a big figure" (make 'big-figure))

  ("frac" "Make fraction" (make-fraction))
  ("sqrt" "Make square root" (make-sqrt))
  ("not" "Make negation" (make-neg))
  ("acute" "Make acute" (make-wide "<acute>"))
  ("bar" "Make bar" (make-wide "<bar>"))
  ("breve" "Make breve" (make-wide "<breve>"))
  ("invbreve" "Make inverted breve" (make-wide "<invbreve>"))
  ("check" "Make check" (make-wide "<check>"))
  ("grave" "Make grave" (make-wide "<grave>"))
  ("hat" "Make hat" (make-wide "^"))
  ("tilde" "Make tilde" (make-wide "~"))
  ("vect" "Make vector" (make-wide "<vect>"))
  ("dot" "Make dot" (make-wide "<dot>"))
  ("ddot" "Make double dot" (make-wide "<ddot>"))
  ("dddot" "Make triple dot" (make-wide "<dddot>"))
  ("ddddot" "Make quadruple dot" (make-wide "<ddddot>"))
  ("widecheck" "Make check" (make-wide "<check>"))
  ("widehat" "Make hat" (make-wide "^"))
  ("widetilde" "Make tilde" (make-wide "~"))

  ("sum" "Insert big summation" (math-big-operator "sum"))
  ("prod" "Insert big product" (math-big-operator "prod"))
  ("coprod" "Insert big coproduct" (math-big-operator "amalg"))
  ("int" "Insert big integral" (math-big-operator "int"))
  ("iint" "Insert big integrals" (math-big-operator "iint"))
  ("iiint" "Insert big integrals" (math-big-operator "iiint"))
  ("oint" "Insert big contour integral" (math-big-operator "oint"))
  ("oiint" "Insert big contour integrals" (math-big-operator "oiint"))
  ("oiiint" "Insert big contour integrals" (math-big-operator "oiiint"))
  ("bigcap" "Insert big intersection" (math-big-operator "cap"))
  ("bigcup" "Insert big union" (math-big-operator "cup"))
  ("bigsqcup" "Insert big square union" (math-big-operator "sqcup"))
  ("bigvee" "Insert big logical or" (math-big-operator "vee"))
  ("bigwedge" "Insert big logical and" (math-big-operator "wedge"))
  ("bigodot" "Insert big dotted point" (math-big-operator "odot"))
  ("bigotimes" "Insert big tensor product" (math-big-operator "otimes"))
  ("bigoplus" "Insert big direct sum" (math-big-operator "oplus"))
  ("biguplus" "Insert big union sum" (math-big-operator "uplus"))

  ("left(" "Insert large (" (math-bracket-open "(" ")" #t))
  ("left)" "Insert large left )" (math-bracket-open ")" "(" #t))
  ("left[" "Insert large [" (math-bracket-open "[" "]" #t))
  ("left]" "Insert large left ]" (math-bracket-open "]" "[" #t))
  ("left{" "Insert large {" (math-bracket-open "{" "}" #t))
  ("left}" "Insert large left }" (math-bracket-open "}" "{" #t))
  ("leftlfloor" "Insert large <lfloor>"
   (math-bracket-open "lfloor" "rfloor" #t))
  ("leftrfloor" "Insert large left <rfloor>"
   (math-bracket-open "rfloor" "lfloor" #t))
  ("leftlceil" "Insert large <lceil>"
   (math-bracket-open "lceil" "rceil" #t))
  ("leftrceil" "Insert large left <rceil>"
   (math-bracket-open "rceil" "lceil" #t))
  ("leftlangle" "Insert large <langle>"
   (math-bracket-open "langle" "rangle" #t))
  ("leftrangle" "Insert large left <rangle>"
   (math-bracket-open "rangle" "langle" #t))
  ("left|" "Insert large left <mid>" (math-bracket-open "|" "|" #t))
  ("left||" "Insert large left <parallel>" (math-bracket-open "||" "||" #t))
  ("left/" "Insert large left /" (math-bracket-open "/" "\\" #t))
  ("leftbackslash" "Insert large left \\" (math-bracket-open "\\" "/" #t))
  ("left." "Insert large left ." (math-bracket-open "." "." #t))

  ("right(" "Insert large right (" (math-bracket-close "(" ")" #t))
  ("right)" "Insert large )" (math-bracket-close ")" "(" #t))
  ("right[" "Insert large right [" (math-bracket-close "[" "]" #t))
  ("right]" "Insert large ]" (math-bracket-close "]" "[" #t))
  ("right{" "Insert large right {" (math-bracket-close "{" "}" #t))
  ("right}" "Insert large }" (math-bracket-close "}" "{" #t))
  ("rightlfloor" "Insert large right <lfloor>"
   (math-bracket-close "lfloor" "rfloor" #t))
  ("rightrfloor" "Insert large <rfloor>"
   (math-bracket-close "rfloor" "lfloor" #t))
  ("rightlceil" "Insert large right <lceil>"
   (math-bracket-close "lceil" "rceil" #t))
  ("rightrceil" "Insert large <rceil>"
   (math-bracket-close "rceil" "lceil" #t))
  ("rightlangle" "Insert large right <langle>"
   (math-bracket-close "langle" "rangle" #t))
  ("rightrangle" "Insert large <rangle>"
   (math-bracket-close "rangle" "langle" #t))
  ("right|" "Insert large right <mid>" (math-bracket-close "|" "|" #t))
  ("right||" "Insert large right <parallel>" (math-bracket-close "||" "||" #t))
  ("right/" "Insert large right /" (math-bracket-close "/" "\\" #t))
  ("rightbackslash" "Insert large right \\" (math-bracket-close "\\" "/" #t))
  ("right." "Insert large right ." (math-bracket-close "." "." #t))

  ("rmfamily" "Use roman font family" (make-with "font-family" "rm"))
  ("ttfamily" "Use typewriter font family" (make-with "font-family" "tt"))
  ("sffamily" "Use sans serif font family" (make-with "font-family" "ss"))
  ("mdseries" "Use medium font series" (make-with "font-series" "medium"))
  ("bfseries" "Use bold font series" (make-with "font-series" "bold"))
  ("upshape" "Use right font shape" (make-with "font-shape" "right"))
  ("itshape" "Use italic font shape" (make-with "font-shape" "italic"))
  ("slshape" "Use slanted font shape" (make-with "font-shape" "slanted"))
  ("scshape" "Use small-caps font shape"
   (make-with "font-shape" "small-caps"))
  ("rm" "Use roman font family" (make-with "font-family" "rm"))
  ("tt" "Use typewriter font family" (make 'tt))
  ("sf" "Use sans serif font family" (make-with "font-family" "ss"))
  ("md" "Use medium font series" (make-with "font-series" "medium"))
  ("bf" "Use bold font series" (make-with "font-series" "bold"))
  ("up" "Use right font shape" (make-with "font-shape" "right"))
  ("it" "Use italic font shape" (make-with "font-shape" "italic"))
  ("em" "Emphasize text" (make 'em))
  ("sl" "Use slanted font shape" (make-with "font-shape" "slanted"))
  ("sc" "Use small-caps font shape" (make-with "font-shape" "small-caps"))
  ("textrm" "Use roman font family" (make-with "font-family" "rm"))
  ("texttt" "Use typewriter font family" (make-with "font-family" "tt"))
  ("textsf" "Use sans serif font family" (make-with "font-family" "ss"))
  ("textmd" "Use medium font series" (make-with "font-series" "medium"))
  ("textbf" "Use bold font series" (make-with "font-series" "bold"))
  ("textup" "Use right font shape" (make-with "font-shape" "right"))
  ("textit" "Use italic font shape" (make-with "font-shape" "italic"))
  ("emph" "Use italic font shape" (make-with "font-shape" "italic"))
  ("textsl" "Use slanted font shape" (make-with "font-shape" "slanted"))
  ("textsc" "Use small-caps font shape"
   (make-with "font-shape" "small-caps"))
  ("cal" "Use calligraphic font" (make-with "math-font" "cal"))
  ("frak" "Use fraktur font" (make-with "math-font" "Euler"))
  ("Bbb" "Use blackboard bold font" (make-with "math-font" "Bbb*"))
  ("mathcal" "Use calligraphic font" (make-with "math-font" "cal"))
  ("mathfrak" "Use fraktur font" (make-with "math-font" "Euler"))
  ("mathbb" "Use blackboard math font" (make-with "math-font" "Bbb*"))
  ("mathrm" "Use roman font family" (make-with "math-font-family" "rm"))
  ("mathtt" "Use typewriter font family" (make-with "math-font-family" "tt"))
  ("mathsf" "Use sans serif font family" (make-with "math-font-family" "ss"))
  ("mathmd" "Use medium font series" (make-with "math-font-series" "medium"))
  ("mathbf" "Use bold font series" (make-with "math-font-series" "bold"))
  ("mathup" "Use right font shape" (make-with "math-font-shape" "right"))
  ("mathit" "Use italic font shape" (make-with "math-font-shape" "italic"))
  ("tiny" "Use tiny font font size" (make 'tiny))
  ("scriptsize" "Use script font size" (make 'very-small))
  ("footnotesize" "Use footnote font size" (make 'smaller))
  ("small" "Use small font size" (make 'small))
  ("normalsize" "Use normal font size" (make 'normal-size))
  ("large" "Use large font size" (make 'large))
  ("Large" "Use very large font size" (make 'larger))
  ("LARGE" "Use very large font size" (make 'very-large))
  ("huge" "Use huge font size" (make 'huge))
  ("Huge" "Use really huge font size" (make 'really-huge))
  ("displaystyle" "Switch to formula style"
   (make-with "math-display" "true"))

  ("black" "Use a black color" (make-with "color" "black"))
  ("white" "Use a white color" (make-with "color" "white"))
  ("grey" "Use a grey color" (make-with "color" "grey"))
  ("red" "Use a red color" (make-with "color" "red"))
  ("blue" "Use a blue color" (make-with "color" "blue"))
  ("yellow" "Use a yellow color" (make-with "color" "yellow"))
  ("green" "Use a green color" (make-with "color" "green"))
  ("orange" "Use a orange color" (make-with "color" "orange"))
  ("magenta" "Use a magenta color" (make-with "color" "magenta"))
  ("brown" "Use a brown color" (make-with "color" "brown"))
  ("pink" "Use a pink color" (make-with "color" "pink"))

  ("smallskip" "Insert small vertical space" (make-vspace-after "0.5fn"))
  ("medskip" "Insert medium vertical space" (make-vspace-after "1fn"))
  ("bigskip" "Insert big vertical space" (make-vspace-after "2fn"))
  ("item" "Insert new item or number" (make-item))
  ("label" "Make label" (make-label))
  ("ref" "Make reference" (make 'reference))
  ("pageref" "Make page reference" (make 'pageref))
  ("footnote" "Insert a footnote" (make-wrapped 'footnote))
  ("input" "Include a document" (make 'include)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions to LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-commands
  ("big." "Insert big ." (math-big-operator "."))
  ("underbrace" "Insert underbrace" (make-wide-under "<wide-underbrace>"))
  ("overbrace" "Insert overbrace" (make-wide "<wide-overbrace>")))

(kbd-symbols
  "ddots" "mho" "Backepsilon" "homsim"
  "mathcatalan" "mathD" "mathd" "mathe" "mathi"
  "mathGamma" "mathLaplace" "matheuler" "mathlambda" "mathpi")
