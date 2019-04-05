;; Conversion between TeXmacs universal symbols and Unicode

;; (C) 2002-2003  Felix Breuer, David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

;; Symbols are sorted by Unicode value and grouped by Unicode block.
;;
;; When the codepage grouping conflict with the logical grouping (e.g.
;; Mathematical Alphanumeric Symbols which are present in the Letterlike
;; Symbols block), the Unicode ordering is preserved but the translation pair
;; is also placed in a comment in the logical group.
;;
;; The intent is making it easy to refer to the Unicode charts, and ensure that
;; logical groups are complete and correct.

;; General Remark about Unicode symbols
;;
;; Most Unicode symbols are intentionally defined as shape-based
;; instead of meaning based. For example U+22C8 (bowtie) maps to
;; symbol <join> and U+2A1D (join, large bowtie) maps to symbol
;; <Bowtie> because <join> is the small variant of <Bowtie>.


;;; Symbols which do not seem to exist in Unicode

;; No appropriate "sans-serif capital y"
;("<Ydown>"	"")
;("<Yleft>"	"")
;("<Yright>"	"")

;; No appropriate "arrow with hook"
;("<hookuparrow>"		"")
;("<hookdownarrow>"		"")
;("<longhookuparrow>"		"")
;("<longhookdownarrow>"		"")

;; No "open-headed arrow from bar"
;("<mapstotriangle>"		"")
;("<longmapstotriangle>"	"")

;; What is Varupsilon?
;("<b-Varupsilon>"	"")

;;; Symbols for internal use with a combining character equivalent

;; No hook character (may use combining char?)
;("<lefthook>"	"")
;("<righthook>"	"")

;; Negation
;("<negate>"	"")
;("<arrownot>"	"")
;("<Arrownot>"	"")

;("<boxempty>"	"")

;;; Symbols for internal use with no Unicode equivalent

;; No component for horizontal curly brackets
;("<braceld>"	"")
;("<bracerd>"	"")
;("<bracelu>"	"")
;("<braceru>"	"")

;; No dotless j
;("<jmath>"	"") -> &jmath;
;("<j*>"	"")

; <vartimes>
; maybe glyph variant of times
; or saltyre (U+2613)
; or n-ary times operator (U+2A09)

;;; Symbols which are _maybe_ synonymous for other symbols
;("<vernal>"	"#2648") ; see <aries>


;;; Big operators and delimiters

;; Most big symbols have no natural translation in Unicode.
;;
;; Also, they should not be used in normal document (instead, they are
;; used internally by the BIG, LEFT, RIGHT primitives).
;;
;; So, they should probably be treated specially (maybe using private
;; use characters during the conversion).

;; Large symbols may be converted to unicode
;("<large-/-0>"		"#29F8") ; big solidus
;("<large-\\-0>"	"#29F9") ; big reverse solidus
;("<large-/-0>"		"/")
;("<large-\\-0"		"#2216") ; see <setminus>

;; Large symbols which have an _approximative_ unicode translation
;("<wilde-tilde>"	"#02DC") ; small tilde
;("<wide-hat>"		"#02C6") ; modifier letter circumflex accent


;; Other problematic symbols

;; is it a variant of <leftsquigarrow> or the actual "leftwards wave arrow"?
;("<wasyleadsto>"	"#219C")

;;; Negated symbols with no aggregated character
;("<nsqsubset>"		"#228F#0338")
;("<nsqsupset>"		"#2290#0338")
;("<nll>"		"#226A#0338")
;("<ngg>"		"#226B#0338")
;("<nlll>"		"#22D8#0338")
;("<nggg>"		"#22D9#0338")
;("<nleqslant>"		"#2A7D#0338")
;("<ngeqslant>"		"#2A7E#0338")
;("<npreceq>"		"#2AAF#0338")
;("<nsucceq>"		"#2AB0#0338")
;("<nleqq>"		"#2266#0338")
;("<ngeqq>"		"#2267#0338")
;("<nsubseteqq>"		"#2AC5#0338")
;("<nsupseteqq>"		"#2AC6#0338")

;;; Glyph variants supported by Unicode
;("<lvertneqq>"		"#2268#FE00") ; variant of <lneqq>
;("<gvertneqq>"		"#2269#FE00") ; variant of <gneqq>
;("<varsubsetneq>"	"#228A#FE00") ; variant of <subsetneq>
;("<varsupsetneq>"	"#228B#FE00") ; variant of <supsetneq>
;("<varsubsetneqq>"	"#2ACB#FE00") ; variant of <subsetneqq>
;("<varsupsetneqq>"	"#2ACC#FE00") ; variant of <supsetneqq>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Control and Basic Latin			0000--007F

("<less>"	"#3C")	; overrides corktounicode
("<gtr>"	"#3E")	; overrides corktounicode
;("<backslash>"	"#5C")  ; see symbol-unicode-math.scm


;;; Controls and Latin-1 Supplement		0080--00FF

; 0080--009F are control chars (several of would be useful in texmacs)
("<varspace>"        "#A0") ; no break space
;("<flip-!>"          "#A1") ; see symbol-unicode-math.scm
("<cent>"            "#A2")
("<sterling>"        "#A3") ; pound sign (for retrocompatibility)
("#BF"               "#A3") ; pound sign (TODO: move this to upgradetm.cpp)
("<currency>"        "#A4")
("<yen>"             "#A5")
("<brokenvert>"      "#A6")
;("<paragraph>"       "#A7") ; see symbol-unicode-math.scm
;("<ddot>"            "#A8") ; see symbol-unicode-math.scm
("<copyright>"       "#A9")
("<ordfeminine>"     "#AA")
("<guillemotleft>"   "#AB") ; (for retrocompatibility)
("#13"               "#AB") ; left pointing double angle quotation mark
                            ; (TODO: move this to upgradetm.cpp)
("<neg>"             "#AC")
("<hyphen>"          "#AD")
("<circledR>"        "#AE") ; for MathML compatility. Maybe U+24C7 more correct.
;("<bar>"	"#AF") ; see symbol-unicode-math.scm
("<degree>"          "#B0")
("<pm>"              "#B1")
("<twosuperior>"     "#B2") ; superscript two
("<threesuperior>"   "#B3") ; superscript three
;("<acute>"	"#B4") ; see symbol-unicode-math.scm
("<mu>"              "#B5") ; micro sign
("<paragraph>"       "#B6")
("<centerdot>"       "#B7")
;("<cedille>"         "#B8") ; see symbol-unicode-math.scm
("<onesuperior>"     "#B9") ; superscript one
("<masculine>"       "#BA") ; masculine ordinal indicator
("<onequarter>"      "#BC") ; vulgar fraction one quarter
("<guillemotright>"  "#BB") ; (for retrocompatibility)
("#14"               "#BB") ; right pointing double angle quotation mark
                            ; (TODO: move this to upgradetm.cpp)
("<onehalf>"         "#BD") ; vulgar fraction one half
("<threequarters>"   "#BE") ; vulgar fraction three quarters
;("<flip-?>"          "#BF") ; see symbol-unicode-math.scm
;("<AA>"              "#C5") ; see symbol-unicode-math.scm
;("<AE>"              "#C6") ; see symbol-unicode-math.scm
;("<Thorn>"           "#DE") ; see symbol-unicode-math.scm
("<times>"            "#D7")
;("<O/>"              "#D8") ; see symbol-unicode-math.scm
;("<sz>"              "#DF") ; see symbol-unicode-math.scm
;("<aa>"              "#E5") ; see symbol-unicode-math.scm
;("<ae>"              "#E6") ; see symbol-unicode-math.scm
;("<dh>"              "#F0") ; see symbol-unicode-math.scm
("<div>"              "#F7")
;("<thorn>"            "#FE") ; see symbol-unicode-math.scm


;;; Latin Extended-A				0100--017F

; h with stroke "#0127"  ; not <hbar> (plank constant)
;("<OE>"	"#0152") ; see symbol-unicode-math.scm
;("<oe>"	"#0153") ; see symbol-unicode-math.scm


;;; Spacing modifier letters			02B0--02FF

;("<breve>"	"#02D8") ; see symbol-unicode-math.scm
;("<dot>"	"#02D9") ; see symbol-unicode-math.scm
;("<abovering>"	"#02DA") ; see symbol-unicode-math.scm


;;; General Punctuation				2000--206F

;; Dashes
;("<-->"	"#0213") ; see symbol-unicode-math.scm
;("<--->"	"#0214") ; see symbol-unicode-math.scm

;; General punctuation
("<nbhyph>"	"#2011")
("<||>"		"#2016")
;("<``>"	"#201C") ; see symbol-unicode-math.scm
("<dagger>"	"#2020")
("<ddagger>"	"#2021")
("<bullet>"	"#2022")

;; General punctuation
("..."	        "#2026")
;("<permil>"	"#2030") ; see symbol-unicode-math.scm
("<prime>"	"#2032")
("<backprime>"	"#2035")

;; Invisible operators
("<nospace>"    "#2061")
("<notimes>"    "#2062")
("<nocomma>"    "#2063")
("<noplus>"     "#2064")

;;; Letterlike Symbols				2100--214F

;; Letterlike symbols
; account of	"#2100"
; addressed to the subject "#2101"
("<bbb-C>"	"#2102")
; degree celsius "#2103"
; centre line symbol "#2104"
; care of	"#2105"
; cada una	"#2106"
; euler constant "#2107"
; scruple	"#2108"
; degree fahrenheit "#2109"
("<cal-g>"	"#210A")
("<cal-H>"	"#210B")
("<frak-H>"	"#210C")
("<bbb-H>"	"#210D")
; planck constant "#210E"
("<hslash>"	"#210F")
("<cal-I>"	"#2110")
("<frak-I>"	"#2111")
("<cal-L>"	"#2112")
("<ell>"	"#2113")
; l b bar symbol "#2114"
("<bbb-N>"	"#2115")
; numero sign	"#2116"
; sound recording copyright "#2117"
("<wp>" 	"#2118")
("<bbb-P>"	"#2119")
("<bbb-Q>"	"#211A")
("<cal-R>"	"#211B")
("<frak-R>"	"#211C")
("<bbb-R>"	"#211D")
; prescription take "#211E"
; response	"#211F"
; service mark	"#2120"
; telephone sign "#2121"
("<trademark>"	"#2122")
; versicle	"#2123"
("<bbb-Z>"	"#2124")
; ounce sign	"#2125"
; ohm sign	"#2126"  ; preferred: #03A9
("<Mho>"	"#2127")
("<frak-Z>"	"#2128")
; turned greek small letter iota "#2129"
; kelvin sign	"#212A"
; Angstrom sign "#212B"  ; preferred: #00C5
("<cal-B>"	"#212C")
("<frak-C>"	"#212D")
; estimated symbol "#212E"
("<cal-e>"	"#212F")
("<cal-E>"	"#2130")
("<cal-F>"	"#2131")
; turned capital f "#2132"
("<cal-M>"	"#2133")
("<cal-o>"	"#2134")

;; Hebrew letterlike math symbols
("<aleph>"	"#2135")
("<beth>"	"#2136")
("<gimel>"	"#2137")
("<daleth>"	"#2138")

;; Additional letterlike symbols
; information source	"#2139"
; rotated capital q	"#213A"
; facsimile sign	"#213B"
("<mathpi*>"	  "#213C")

;;; Arrows					2190--21FF

("<leftarrow>"		"#2190")
("<uparrow>"		"#2191")
("<rightarrow>"		"#2192")
("<downarrow>"		"#2193")
("<leftrightarrow>"	"#2194")
("<updownarrow>"	"#2195")
("<nwarrow>"		"#2196")
("<nearrow>"		"#2197")
("<searrow>"		"#2198")
("<swarrow>"		"#2199")
("<nleftarrow>"		"#219A")
("<nrightarrow>"	"#219B")
("<leftsquigarrow>" 	"#219C") ; nominally "leftwards wave arrow"
("<rightsquigarrow>" 	"#219D") ; nominally "rightwards wave arrow"
("<twoheadleftarrow>"	"#219E")
; <twoheaduparrow>	"#219F"
("<twoheadrightarrow>"	"#21A0")
; <twoheaddownarrow>	"#21A1"
("<leftarrowtail>"	"#21A2")
("<rightarrowtail>"	"#21A3")
("<mapsfrom>"		"#21A4")
; upwards arrow from bar "#21A5"
("<mapsto>"		"#21A6")
; downwards arrow from bar "#21A7"
("<hookleftarrow>"	"#21A9")
("<hookrightarrow>"	"#21AA")
("<looparrowleft>"	"#21AB")
("<looparrowright>"	"#21AC")
("<leftrightsquigarrow>" "#21AD") ; nominally "left right wave arrow"
("<nleftrightarrow>"	"#21AE")
("<lightning>"		"#21AF")
("<Lsh>"		"#21B0")
("<Rsh>"		"#21B1")
; downwards arrow with tip leftwards "#21B2"
; downwards arrow with tip rightwards "#21B3"
; rightwards arrow with corner downwards "#21B4"
; downwards arrow with corner leftwards "#21B5"
("<curvearrowleft>"	"#21B6")
("<curvearrowright>"	"#21B7")
; north west arrow to long bar "#21B8"
; leftwards arrow to bar over rightwards arrow to bar "#21B9"
("<circlearrowleft>"	"#21BA")
("<circlearrowright>"	"#21BB")
("<leftharpoonup>"	"#21BC")
("<leftharpoondown>"	"#21BD")
("<upharpoonright>"	"#21BE")
("<upharpoonleft>"	"#21BF")
("<rightharpoonup>"	"#21C0")
("<rightharpoondown>"	"#21C1")
("<downharpoonright>"	"#21C2")
("<downharpoonleft>"	"#21C3")
("<rightleftarrows>"	"#21C4")
; <updownarrows>	"#21C5"
("<leftrightarrows>"	"#21C6")
("<leftleftarrows>"	"#21C7")
("<upuparrows>"		"#21C8")
("<rightrightarrows>"	"#21C9")
("<downdownarrows>"	"#21CA")
("<leftrightharpoons>"	"#21CB")
("<rightleftharpoons>"	"#21CC")
("<nLeftarrow>"		"#21CD")
("<nLeftrightarrow>"	"#21CE")
("<nRightarrow>"	"#21CF")
("<Leftarrow>"		"#21D0")
("<Uparrow>"		"#21D1")
("<Rightarrow>"		"#21D2")
("<Downarrow>"		"#21D3")
("<Leftrightarrow>"	"#21D4")
("<Updownarrow>"	"#21D5")
; <Nwarrow>		"#21D6"
; <Nearrow>		"#21D7"
; <Searrow>		"#21D8"
; <Swarrow>		"#21D9"
("<Lleftarrow>"		"#21DA")
("<Rrightarrow>"	"#21DB")
; leftwards squiggle arrow "#21DC"  ; maybe <leftsquigarrow>
; rigthwards squiggle arrow "#21DD" ; maybe <rightsquigarrow>

("<leftarrowtriangle>" "#21FD")
("<rightarrowtriangle>" "#21FE")
("<leftrightarrowtriangle>" "#21FF")

;;; Supplemental Arrows-A			27F0--27FF

;; Arrows
; upwards quadruple arrow		"#27F0"
; downwards quadruple arrow		"#27F1"
; anticlockwise gapped circle arrow	"#27F2"
; clockwise gapped circle arrow		"#27F3"
; right arrow with circled plus		"#27F4"

;; Long arrows
;; According to Unicode: "The long arrows are used for mapping whereas
;; the short forms would be used in limits.
("<longleftarrow>"	"#27F5")
("<longrightarrow>"	"#27F6")
("<longleftrightarrow>"	"#27F7")
("<Longleftarrow>"	"#27F8")
("<Longrightarrow>"	"#27F9")
("<Longleftrightarrow>"	"#27FA")
("<longmapsfrom>"	"#27FB")
("<longmapsto>"		"#27FC")
; long leftwards double arrow from bar	"#27FD"
; long rightwards double arrow from bar	"#27FE"
; long rightwards squiggle arrow	"#27FF"


;;; Mathematical Operators			2200--22FF

("<forall>"		"#2200")
("<complement>"		"#2201")
("<partial>"		"#2202")
("<exists>"		"#2203")
("<nexists>"		"#2204")
("<emptyset>"		"#2205")
; increment		"#2206"
("<nabla>"		"#2207")
("<in>"			"#2208")
("<nin>"		"#2209")
; small element of	"#220A"
("<ni>"			"#220B")
("<nni>"		"#220C")
; smallmath contains as member "#220D"
; end of proof		"#220E"
("<big-prod>"		"#220F")
("<big-amalg>"          "#2210")
("<big-sum>"		"#2211")
("<minus>"		"#2212")
("<mp>"			"#2213")
("<dotplus>"		"#2214")
; division slash	"#2215"
("<setminus>"		"#2216")
("<ast>"		"#2217")
("<circ>"		"#2218")
; bullet operator	"#2219"  ; not <bullet> punctuation
("<sqrt>"		"#221A") ; FIXME: wrong baseline
; cube root		"#221B"
; fourth root		"#221C"
("<propto>"		"#221D")
("<infty>"		"#221E")
; right angle		"#221F"
("<angle>"		"#2220")
("<measuredangle>"	"#2221")
("<sphericalangle>"	"#2222")
("<mid>"		"#2223")
("<nmid>"		"#2224")
("<parallel>"		"#2225")
("<nparallel>"		"#2226")
("<wedge>"		"#2227")
("<vee>"		"#2228")
("<cap>"		"#2229")
("<cup>"		"#222A")
("<big-int>"		"#222B")
("<big-iint>"		"#222C")
("<big-iiint>"		"#222D")
("<big-oint>"		"#222E")
("<big-oiint>"		"#222F")
("<big-oiiint>"		"#2230")
; clockwise integral	"#2231"
; clockwise contour integral "#2232"
; anticlockwise contour integral "#2233"
("<therefore>"		"#2234")
("<because>"		"#2235")
; ratio			"#2236"
; proportion		"#2237"
; dot minus		"#2238"
; excess		"#2239"
; geometric proportion	"#223A"
; homothetic		"#223B"
("<sim>"		"#223C")
("<backsim>"		"#223D")
; inverted lazy s	"#223E"
; sine wave		"#223F"
("<wr>"			"#2240")
("<nsim>"		"#2241")
("<eqsim>"		"#2242")
("<simeq>"		"#2243")
("<nsimeq>"		"#2244")
("<cong>"		"#2245")
; approximately but not actually equal to "#2246"
("<ncong>"		"#2247")
("<approx>"		"#2248")
("<napprox>"		"#2249")
("<approxeq>"		"#224A")
; triple tilde		"#225B"
; all equal to		"#225C"
("<asymp>"		"#224D")
("<Bumpeq>"		"#224E")
("<bumpeq>"		"#224F")
("<doteq>"		"#2250")
("<doteqdot>"		"#2251")
("<fallingdoteq>"	"#2252")
("<risingdoteq>"	"#2253")
("<assign>"		"#2254")
; equals colon		"#2255"
("<eqcirc>"		"#2256")
("<circeq>"		"#2257")
; corresponds to	"#2258"
; estimates		"#2259"
; equiangular to	"#225A"
; star equals		"#225B"
("<triangleq>"		"#225C")
; equal to by definition "#225D"
; measured by		"#225E"
; questioned equal to	"#225F"
("<neq>"		"#2260")
("<equiv>"		"#2261")
("<nequiv>"		"#2262")
; strictly equivalent to "#2263"
("<leq>"		"#2264")
("<geq>"		"#2265")
("<leqq>"		"#2266")
("<geqq>"		"#2267")
("<lneqq>"		"#2268")
("<gneqq>"		"#2269")
("<ll>"			"#226A")
("<gg>"			"#226B")
("<between>"		"#226C")
("<nasymp>"		"#226D")
("<nless>"		"#226E")
("<ngtr>"		"#226F")
("<nleq>"		"#2270")
("<ngeq>"		"#2271")
("<lesssim>"		"#2272")
("<gtrsim>"		"#2273")
; <nlesssim>		"#2274"
; <ngtrsim>		"#2275"
("<lessgtr>"		"#2276")
("<gtrless>"		"#2277")
; <nlessgtr>		"#2278"
; <ngtrless>		"#2279"
("<prec>"		"#227A")
("<succ>"		"#227B")
("<preccurlyeq>"	"#227C")
("<succcurlyeq>"	"#227D")
("<precsim>"		"#227E")
("<succsim>"		"#227F")
("<nprec>"		"#2280")
("<nsucc>"		"#2281")
("<subset>"		"#2282")
("<supset>"		"#2283")
("<nsubset>"		"#2284")
("<nsupset>"		"#2285")
("<subseteq>"		"#2286")
("<supseteq>"		"#2287")
("<nsubseteq>"		"#2288")
("<nsupseteq>"		"#2289")
("<subsetneq>"		"#228A")
("<supsetneq>"		"#228B")
; multiset (<uleftarrow>) "#228C"
; MULTISET MULTIPLICATION (<udot> "#228D"
("<uplus>"		"#228E")
("<sqsubset>"		"#228F")
("<sqsupset>"		"#2290")
("<sqsubseteq>"		"#2291")
("<sqsupseteq>"		"#2292")
("<sqcap>"		"#2293")
("<sqcup>"		"#2294")
; FIXME: o* symbols have a too thin circle line, varo* symbols are correct
("<varoplus>"		"#2295") ; FIXME: swap oplus and varoplus
("<varominus>"		"#2296") ; FIXME: swap ominus and varominus
("<varotimes>"		"#2297") ; FIXME: swap otimes and varotimes
("<varoslash>"		"#2298") ; FIXME: swap oslash and varoslash
("<varodot>"		"#2299") ; FIXME: swap odot and varodot
; FIXME: rename circledcirc to varocircle
("<varocircle>"		"#229A") ; FIXME: rename varocircle to ocircle
; FIXME: rename circledast to varoast
("<varoast>"		"#229B") ; FIXME: rename varoast to oast
; circled equals	"#229C"
; FIXME: define symbol odash with correct circle line width
("<circleddash>"	"#229D") ; FIXME: use odash instead
("<boxplus>"		"#229E")
("<boxminus>"		"#229F")
("<boxtimes>"		"#22A0")
("<boxdot>"		"#22A1")

;; <vdash>-like symbols are a bit problematic
;; These mappings provide a better visual match, but do map some
;; "simple" symbols like <dashv> or <vDash> and would cause more
;; problems since there are no <nlong*> symbols.
;("<longvdash>"		"#22A2") ; RIGHT TACK	as long as TRUE (proves)
;("<longdashv>"		"#22A3") ; LEFT TACK	as long as TRUE (yields)
;("<top>"		"#22A4")
;("<bot>"		"#22A5") ; TODO: <perp> -> #22A5
;("<vdash>"		"#22A6") ; ASSERTION	should be as long as MODELS
;("<models>"		"#22A7") ; vDash, shorter than TRUE
;("<longvDash>		"#22A8") ; TRUE
;("<Vdash>"		"#22A9") ; FORCES
;("<Vvdash>"		"#22AA")
;; <VDash>		"#22AB"  ; maybe longVDash (as long as TRUE)

;; These mappings do not define ASSERTION and make MODELS use the same
;; glyph as TRUE, but cause fewer problems otherwise and use (mostly)
;; the same mappings as Omega.
("<vdash>"		"#22A2") ; RIGHT TACK	as long as TRUE (proves)
("<dashv>"		"#22A3") ; LEFT TACK	as long as TRUE (yields)
("<top>"		"#22A4")
("<bot>"		"#22A5")
; assertion		"#22A6"  ; ASSERTION	should be as long as MODELS
("<models>"		"#22A7") ; MODELS	should be shorter than TRUE
("<vDash>"		"#22A8") ; TRUE		should be longer than MODELS
("<Vdash>"		"#22A9") ; FORCES
("<Vvdash>"		"#22AA")
; <VDash>		"#22AB"

("<nvdash>"		"#22AC") ; DOES NOT PROVE
("<nvDash>"		"#22AD") ; NOT TRUE
("<nVdash>"		"#22AE") ; DOES NOT FORCE
("<nVDash>"		"#22AF")
; precedes under relation "#22B0"
; succeeds under relation "#22B1"
("<vartriangleleft>"	"#22B2")
("<vartriangleright>"	"#22B3")
("<trianglelefteq>"	"#22B4")
("<trianglerighteq>"	"#22B5")
; original of		"#22B6"
; image of		"#22B7"
("<multimap>"		"#22B8")
; hermitian conjugate matrix "#22B9"
("<intercal>"		"#22BA")
("<veebar>"		"#22BB")
; nand			"#22BC") ; see <barwedge>
; <barvee>		"#22BD"
; right angle with arc	"#22BE"
; right triangle	"#22BF"
("<big-wedge>"	        "#22C0")
("<big-vee>"		"#22C1")
("<big-cap>"		"#22C2")
("<big-cup>"		"#22C3")
("<diamond>"		"#22C4") ; TODO: define as arithmetic-times
("<cdot>"		"#22C5") ; not MIDDLE DOT
("<star>"		"#22C6")
("<divideontimes>"	"#22C7")
("<join>"		"#22C8")
("<ltimes>"		"#22C9")
("<rtimes>"		"#22CA")
("<leftthreetimes>"	"#22CB")
("<rightthreetimes>"	"#22CC")
("<backsimeq>"		"#22CD")
("<curlyvee>"		"#22CE")
("<curlywedge>"		"#22CF")
("<Subset>"		"#22D0")
("<Supset>"		"#22D1")
("<Cap>"		"#22D2")
("<Cup>"		"#22D3")
("<pitchfork>"		"#22D4")
; equal and parallel to	"#22D5"
("<lessdot>"		"#22D6")
("<gtrdot>"		"#22D7")
("<lll>"		"#22D8")
("<ggg>"		"#22D9")
("<lesseqgtr>"		"#22DA")
("<gtreqless>"		"#22DB")
; <eqless>		"#22DC"
; <eqgtr>		"#22DD"
("<curlyeqprec>"	"#22DE")
("<curlyeqsucc>"	"#22DF")
("<npreccurlyeq>"	"#22E0")
("<nsucccurlyeq>"	"#22E1")
("<nsqsubseteq>"	"#22E2")
("<nsqsupseteq>"	"#22E3")
; <sqsubsetneq>		"#22E4"
; <sqsupsetneq>		"#22E5"
("<lnsim>"		"#22E6")
("<gnsim>"		"#22E7")
("<precnsim>"		"#22E8")
("<succnsim>"		"#22E9")
("<ntriangleleft>"	"#22EA")
("<ntriangleright>"	"#22EB")
("<ntrianglelefteq>"	"#22EC")
("<ntrianglerighteq>"	"#22ED")
("<ldots>"		"#2026")
("<vdots>"		"#22EE")
("<cdots>"		"#22EF")
("<udots>"		"#22F0")
("<ddots>"		"#22F1")
; element of with long horizontal stroke "#22F2"
; element of with vertical bar at end of horizontal stroke "#22F3"
; small element of with vertical bar at end of horizontal stroke "#22F4"
; <dotin>		"#22F5"
; <eqin>		"#22F6"
; small element of with overbar "#22F7"
; <ineq>		"#22F8"
; element of with two horizontal strokes "#22F9"
; contains with long horizontal stroke "#22FA"
; contains with vertical bar at end of horizontal stroke "#22FB"
; small contains with vertical bar at end of horizontal stroke "#22FC"
; <eqni>		"#22FD"
; small contains with overbar "#22FE"
; Z notation bag membership "#22FF"

;;; Miscellaneous Technical			2300--23FF

;; Miscellaneous Technical
("<diameter>"		"#2300")
; electric arrow	"#2301"
; house			"#2302"
; up arrowhead		"#2303"
; down arrowhead	"#2304"
("<barwedge>"		"#2305")
("<doublebarwedge>"	"#2306") ; perspective
; vawy line		"#2307"

;; Corner brackets
("<lceil>"		"#2308")
("<rceil>"		"#2309")
("<lfloor>"		"#230A")
("<rfloor>"		"#230B")

;; Miscellaneous technical
("<invneg>"		"#2310")
; square lozenge	"#2311"
; arc			"#2312"
; segment		"#2313"
; sector		"#2314"
("<recorder>"		"#2315")
; position indicator	"#2316"
; viewdata square	"#2317"
("<place of interest sign>"		"#2318");; Apple command key 
; turned not sign	"#2319"
("<option key>"		"#2325")

;; GUI icons
; watch			"#231A"
; hourglass		"#231B"

;; Quine corners
("<ulcorner>"		"#231C")
("<urcorner>"		"#231D")
("<llcorner>"		"#231E")
("<lrcorner>"		"#231F")

;; Frown and smile
("<frown>"		"#2322")
("<smile>"		"#2323")

;; Angle brackets
;; These are discouraged for mathematical use because of their
;; canonical equivalence to CJK punctuation.
; left-pointing angle bracket	"#2329" ; see <langle>
; right-pointing angle blacket	"#232A" ; see <rangle>

;; APL
; apl functional symbol i-beam			"#2337"
("<talloblong>"					"#2338")
; apl functional symbol quad equal		"#2339"
; apl functional symbol quad equal		"#2338"
; apl functional symbol quad divide		"#2339"
; apl functional symbol quad diamond		"#233A"
; apl functional symbol quad jot		"#233B"
; apl functional symbol quad circle		"#233C"
; apl functional symbol circle stile		"#233D"
; apl functional symbol circle jot		"#233E"
; apl functional symbol slash bar		"#233F"
; apl functional symbol backslash bar		"#2340"
; apl functional symbol quad slash		"#2341"
; apl functional symbol quad backslash		"#2342"
; apl functional symbol quad less-than		"#2343"
; apl functional symbol quad greater-than	"#2344"
; apl functional symbol leftwards vane		"#2345"
; apl functional symbol rightwards vane		"#2346"
("<APLleftarrowbox>"				"#2347")
("<APLrightarrowbox>"				"#2348")
; apl functional symbol circle backslash	"#2349"
; apl functional symbol down tack underbar	"#234A"
; apl functional symbol delta stile		"#234B"
; apl functional symbol quad down caret		"#234C"
; apl functional symbol quad delta		"#234D"
; apl functional symbol down tack jot		"#234E"
; apl functional symbol upwards vane		"#234F"
("<APLuparrowbox>"				"#2350")
; apl functional symbol up tack overbar		"#2351"
; apl functional symbol del stile		"#2352"
; apl functional symbol quad up caret		"#2353"
; apl functional symbol quad del		"#2354"
; apl functional symbol up tack jot		"#2355"
; apl functional symbol downwards vane		"#2356"
("<APLdownarrowbox>"				"#2357")
; apl functional symbol quote underbar		"#2358"
; apl functional symbol delta underbar		"#2359"
; apl functional symbol diamond underbar	"#235A"
; apl functional symbol jot underbar		"#235B"
; apl functional symbol circle underbar		"#235C"
; apl functional symbol up shoe jot		"#235D"
("<APLinput>"					"#235E")
; apl functional symbol circle star		"#235F"
; apl functional symbol quad colon		"#2360"
; apl functional symbol up tack diaeresis	"#2361"
; apl functional symbol del diaeresis		"#2362"
; apl functional symbol star diaeresis		"#2363"
; apl functional symbol jot diaeresis		"#2364"
; apl functional symbol circle diaeresis	"#2365"
; apl functional symbol down shoe stile		"#2366"
; apl functional symbol left shoe stile		"#2367"
; apl functional symbol tilde diaeresis		"#2368"
; apl functional symbol greater-than diaeresis	"#2369"
; apl functional symbol comma bar		"#236A"
; apl functional symbol del tilde		"#236B"
; apl functional symbol zilde			"#236C"
; apl functional symbol stile tilde		"#236D"
; apl functional symbol semicolon underbar	"#236E"
; apl functional symbol quad not equal		"#236F"
; apl functional symbol quad question		"#2370"
; apl functional symbol down caret tilde	"#2371"
; apl functional symbol up caret tilde		"#2372"
; apl functional symbol iota			"#2373"
; apl functional symbol rho			"#2374"
; apl functional symbol omega			"#2375"
; apl functional symbol alpha underbar		"#2376"
; apl functional symbol epsilon underbar	"#2377"
; apl functional symbol iota underbar		"#2378"
; apl functional symbol omega underbar		"#2379"
; apl functional symbol alpha			"#237A"

;; APL
("<APLbox>"				"#2395")


;;; Enclosed Alphanumerics			2460--24FF

("<circledS>"		                "#24C8")


;;; Geometric Shapes				25A0--25FF

;; Geometric Shapes
("<blacksquare>"			"#25A0")
("<Square>"				"#25A1")
; white square with rounded corners		"#25A2"
; white square containing black small square	"#25A3"
; square with horizontal fill			"#25A4"
; square with vertical fill			"#25A5"
; square with orthogonal crosshatch fill	"#25A6"
; square with upper left to lower right fill	"#25A7"
; square with upper right to lower left fill	"#25A8"
; square with diagonal crosshatch fill		"#25A9"
; black small square			"#25AA"
; white small square			"#25AB"
; black rectangle			"#25AC"
; white rectangle			"#25AD"
; black vertical rectangle		"#25AE"
("<oblong>"				"#25AF")
; black parallelogram			"#25B0"
; white parallelogram			"#25B1"
; black up-pointing triangle		"#25B2"
("<bigtriangleup>"	                "#25B3")
("<blacktriangle>"			"#25B4")
("<vartriangle>"			"#25B5")
; black right-pointing triangle		"#25B6"
; white right-pointing triangle		"#25B7"
("<blacktriangleright>"			"#25B8")
("<triangleright>"	                "#25B9")
; black right-pointing pointer		"#25BA"
; white right-pointing pointer		"#25BB"
; black down-pointing triangle		"#25BC"
("<bigtriangledown>"	                "#25BD")
("<blacktriangledown>"			"#25BE")
("<triangledown>"			"#25BF")
; black left-pointing triangle		"#25C0"
; white left-pointing triangle		"#25C1"
("<blacktriangleleft>"			"#25C2")
("<triangleleft>"	                "#25C3")
; black left-pointing pointer		"#25C4"
; white left-pointing pointer		"#25C5"
; black diamond				"#25C6"
("<wasyDiamond>"			"#25C7")
; white diamond containing black small diamond	"#25C8"
; fisheye				"#25C9"
("<lozenge>"		                "#25CA")
("<Circle>"				"#25CB")
; dotted circle				"#25CC"
; circle with vertical fill		"#25CD"
; bullseye				"#25CE"
("<CIRCLE>"				"#25CF")
("<LEFTcircle>"				"#25D0")
("<RIGHTcircle>"			"#25D1")
; circle with lower half black		"#25D2"
; circle with upper half black		"#25D3"
; circle with upper right quadrant black	"#25D4"
; circle with all but upper left quadrant black	"#25D5"
("<LEFTCIRCLE>"				"#25D6")
("<RIGHTCIRCLE>"			"#25D7")
; inverse bullet			"#25D8"
; inverse white circle			"#25D9"
; upper half inverse white circle	"#25DA"
; lower half inverse white circle	"#25DB"
; upper left quadrant circular arc	"#25DC"
; upper right quadrant circular arc	"#25DD"
; lower right quadrant circular arc	"#25DE"
; lower left quadrant circular arc	"#25DF"
; upper half circle			"#25E0"
; lower half circle			"#25E1"
; black lower right triangle		"#25E2"
; black lower left triangle		"#25E3"
; black upper left triangle		"#25E4"
; black upper right triangle		"#25E5"
; white bullet				"#25E6"
; square with left half black		"#25E7"
; square with right half black		"#25E8"
; square with upper left diagonal half black	"#25E9"
; square with lower right diagonal half black	"#25EA"
; white square with vertical bisecting line	"#25EB"
; white up-pointing triangle with dot		"#25EC"
; up-pointing triangle with left half black	"#25ED"
; up-pointing triangle with right half black	"#25EE"
("<varbigcirc>"		"#25EF")

;; Geometric shapes
; upper left triangle			"#25F8"
; upper right triangle			"#25F9"
; lower left triangle			"#25FA"
; white medium square			"#25FB"
; black medium square			"#25FC"
("<square>"				"#25FD") ; smaller than <Square>
; black medium small square		"#25FE"
; lower right triangle			"#25FF"


;;; Miscellaneous Symbols			2600--26FF

;; Weather and astrological symbols
; black sun with rays	"#2600"
; cloud			"#2601"
; umbrella		"#2602"
; snowman		"#2603"
; comet			"#2604"
("<bigstar>"		"#2605")
; white star		"#2606"
; lightning		"#2607"
; thunderstorm		"#2608"
("<astrosun>"		"#2609")
("<ascnode>"		"#260A")
("<descnode>"		"#260B")
("<conjunction>"	"#260C")
("<opposition>"		"#260D")

;; Miscellaneous symbols
("<phone>"		"#260E")
; white telephone	"#260F"
("<wasyBox>"		"#2610")
("<XBox>"		"#2611")
("<CheckedBox>"		"#2612")
; saltire		"#2613"

;; Miscellaneous symbols
; wheel of dharma	"#2638"
("<frownie>"		"#2639")
("<smiley>"		"#263A")
("<blacksmiley>"	"#263B")
("<sun>"		"#263C")

;; Astrological symbols
("<rightmoon>"		"#263D")
("<leftmoon>"		"#263E")
("<mercury>"		"#263F")
("<female>"		"#2640")
("<earth>"		"#2641")
("<male>"		"#2642")
("<jupiter>"		"#2643")
("<saturn>"		"#2644")
("<uranus>"		"#2645")
("<neptune>"		"#2646")
("<pluto>"		"#2647")
("<aries>"		"#2648")
("<taurus>"		"#2649")
("<gemini>"		"#264A")
("<cancer>"		"#264B")
("<leo>"		"#264C")
("<virgo>"		"#264D")
("<libra>"		"#264E")
("<scorpio>"		"#264F")
("<sagittarius>"	"#2650")
("<capricornus>"	"#2651")
("<aquarius>"		"#2652")
("<pisces>"		"#2653")

;; Playing card symbols
("<spadesuit>"		"#2660")
; white heart suit	"#2661"
; white diamond suit	"#2662"
("<clubsuit>"		"#2663")
; white spade suit	"#2664"
("<heartsuit>"		"#2665")
("<diamondsuit>"	"#2666")

;; Musical symbols
("<quarternote>"	"#2669")
("<eighthnote>"		"#266A")
("<twonotes>"		"#266B")
; beamed sixteenth notes "#266C"
("<flat>"		"#266D")
("<natural>"		"#266E")
("<sharp>"		"#266F")


;;; Dingbats					2710--27BF

("<checkmark>"		"#2713")
("<maltese>"		"#2720")
; black diamond minus white x	"#2756"


;;; Supplemental Arrows-B

("<leftturn>"		"#2940") ; not closed, but correct arrowhead position
("<rightturn>"		"#2941") ; not closed, but correct arrowhead position


;;; Miscellaneous Mathematical Symbols-A	27C0--21FF

;; Mathematical brackets
("<llbracket>"		"#27E6")
("<rrbracket>"		"#27E7")
("<langle>"		"#27E8")
("<rangle>"		"#27E9")
("<llangle>"		"#27EA")
("<rrangle>"		"#27EB")


;;; Miscellaneous Mathematical Symbols-B	2980--29FF

;; Miscellaneous mathematical symbols
; triple vertical bar delimiter	"#2980"
; z notation spot		"#2981"
; z notation type colon		"#2982"

;; Brackets
; left white curly bracket		"#2983"
; right white curly bracket		"#2984"
; left white parenthesis		"#2985"
; right white parenthesis		"#2986"
("<llparenthesis>"			"#2987")
("<rrparenthesis>"			"#2988")
; z notation left binding bracket	"#2989"
; z notation right binding bracket	"#298A"
; left square bracket with underbar	"#298B"
; right square bracket with underbar	"#298C"
; left square bracket with tick in top corner		"#298D"
; right square bracket with tick in bottom corner	"#298E"
; left square bracket with tick in bottom corner	"#298F"
; right square bracket with tick in top corner		"#2990"
; left angle bracket with dot		"#2991"
; right angle bracket with dot		"#2992"
; left arc less-than bracket		"#2993"
; right arc greater-than bracket	"#2994"
; double left arc greater-than bracket	"#2995"
; double right arc less-than bracket	"#2996"
; left black tortoise shell bracket	"#2997"
; right black tortoise shell bracket	"#2998"

;; Circle symbols
("<minuso>"		"#29B5")
("<varobar>"		"#29B6")
; circled parallel	"#29B7"
("<varobslash>"		"#29B8")
; circled perpendicular	"#29B9"
; circle divided by horizontal bar and top half divided by vertical bar	"#29BA"
; circle with superimposed x			"#29BB"
; circled anticlockwise-rotated division sign	"#29BC"
; up arrow through circle			"#29BD"
; circled white bullet				"#29BE"
; circled bullet				"#29BF"
; circled less-than				"#29C0"
; circled greater-than				"#29C1"
; circle with small circle to the right		"#29C2"
; circle with two horizontal strokes to the right "#29C3"

;; Square symbols
("<boxslash>"		"#29C4")
("<boxbslash>"		"#29C5")
("<boxast>"		"#29C6")
("<boxcircle>"		"#29C7")
("<boxbox>"		"#29C8")
; two joined squares	"#29C9"



("<blacklozenge>"	"#29EB")

;;; Supplemental Mathematical Operators		2A00--2AFF

("<big-odot>"		"#2A00")
("<big-oplus>"		"#2A01")
("<big-otimes>"		"#2A02")
("<big-pluscup>"	"#2A04")
("<big-sqcap>"   	"#2A05")
("<big-sqcup>"  	"#2A06")
("<merge>"		"#2A07")
("<Bowtie>"		"#2A1D")
("<amalg>"		"#2A3F")
("<leqslant>"		"#2A7D")
("<geqslant>"		"#2A7E")
("<lessapprox>"		"#2A85")
("<gtrapprox>"		"#2A86")
("<lneq>"		"#2A87")
("<gneq>"		"#2A88")
("<lnapprox>"		"#2A89")
("<gnapprox>"		"#2A8A")
("<lesseqqgtr>"		"#2A8B")
("<gtreqqless>"		"#2A8C")
("<eqslantless>"	"#2A95")
("<eqslantgtr>"		"#2A96")
("<leftslice>"		"#2AA6")
("<rightslice>"		"#2AA7")
("<preceq>"		"#2AAF")
("<succeq>"		"#2AB0")
("<precneqq>"		"#2AB5")
("<succneqq>"		"#2AB6")
("<precapprox>"		"#2AB7")
("<succapprox>"		"#2AB8")
("<precnapprox>"	"#2AB9")
("<succnapprox>"	"#2ABA")
("<subseteqq>"		"#2AC5")
("<supseteqq>"		"#2AC6")
("<subsetneqq>"		"#2ACB")
("<supsetneqq>"		"#2ACC")
("<interleave>"		"#2AF4")


;;; Greek and Coptic				0370--03FF

;; Uppercase Greek
("<Alpha>"	"#0391")
("<Beta>"	"#0392")
("<Gamma>"	"#0393")
("<Delta>"	"#0394")
("<Epsilon>"	"#0395")
("<Zeta>"	"#0396")
("<Eta>"	"#0397")
("<Theta>"	"#0398")
("<Iota>"	"#0399")
("<Kappa>"	"#039A")
("<Lambda>"	"#039B")
("<Mu>"		"#039C")
("<Nu>"		"#039D")
("<Xi>"		"#039E")
("<Omicron>"	"#039F")
("<Pi>"		"#03A0")
("<Rho>"	"#03A1")
("<Sigma>"	"#03A3")
("<Tau>"	"#03A4")
("<Upsilon>"	"#03A5")
("<Phi>"	"#03A6")
("<Chi>"	"#03A7")
("<Psi>"	"#03A8")
("<Omega>"	"#03A9")

;; Lowercase Greek
("<alpha>"	"#03B1")
("<beta>"	"#03B2")
("<gamma>"	"#03B3")
("<delta>"	"#03B4")
("<varepsilon>"	"#03B5")
("<zeta>"	"#03B6")
("<eta>"	"#03B7")
("<theta>"	"#03B8")
("<iota>"	"#03B9")
("<kappa>"	"#03BA")
("<lambda>"	"#03BB")
("<mu>"		"#03BC")
("<nu>"		"#03BD")
("<xi>"		"#03BE")
("<omicron>"	"#03BF")
("<pi>"		"#03C0")
("<rho>"	"#03C1")
("<varsigma>"	"#03C2")
("<sigma>"	"#03C3")
("<tau>"	"#03C4")
("<upsilon>"	"#03C5")
("<varphi>"	"#03C6")
("<chi>"	"#03C7")
("<psi>"	"#03C8")
("<omega>"	"#03C9")

;; Variant letterforms
("<vartheta>"	  "#03D1")
("<phi>"	  "#03D5")
("<varpi>"	  "#03D6")
("<digamma>"	  "#03DD")
("<varkappa>"	  "#03F0")
("<varrho>"	  "#03F1")
("<epsilon>"	  "#03F5")
("<backepsilon>"  "#03F6")

;;; Halfwidth and Fullwidth Forms		FF00--FFEF

("<longminus>"	"#FF0D")
("<longequal>"	"#FF1D")

;;; Mathematical Alphanumeric Symbols		1D400--1D7FF

;; Bold upright symbols
("<b-up-A>"	"#1D400")
("<b-up-B>"	"#1D401")
("<b-up-C>"	"#1D402")
("<b-up-D>"	"#1D403")
("<b-up-E>"	"#1D404")
("<b-up-F>"	"#1D405")
("<b-up-G>"	"#1D406")
("<b-up-H>"	"#1D407")
("<b-up-I>"	"#1D408")
("<b-up-J>"	"#1D409")
("<b-up-K>"	"#1D40A")
("<b-up-L>"	"#1D40B")
("<b-up-M>"	"#1D40C")
("<b-up-N>"	"#1D40D")
("<b-up-O>"	"#1D40E")
("<b-up-P>"	"#1D40F")
("<b-up-Q>"	"#1D410")
("<b-up-R>"	"#1D411")
("<b-up-S>"	"#1D412")
("<b-up-T>"	"#1D413")
("<b-up-U>"	"#1D414")
("<b-up-V>"	"#1D415")
("<b-up-W>"	"#1D416")
("<b-up-X>"	"#1D417")
("<b-up-Y>"	"#1D418")
("<b-up-Z>"	"#1D419")
("<b-up-a>"	"#1D41A")
("<b-up-b>"	"#1D41B")
("<b-up-c>"	"#1D41C")
("<b-up-d>"	"#1D41D")
("<b-up-e>"	"#1D41E")
("<b-up-f>"	"#1D41F")
("<b-up-g>"	"#1D420")
("<b-up-h>"	"#1D421")
("<b-up-i>"	"#1D422")
("<b-up-j>"	"#1D423")
("<b-up-k>"	"#1D424")
("<b-up-l>"	"#1D425")
("<b-up-m>"	"#1D426")
("<b-up-n>"	"#1D427")
("<b-up-o>"	"#1D428")
("<b-up-p>"	"#1D429")
("<b-up-q>"	"#1D42A")
("<b-up-r>"	"#1D42B")
("<b-up-s>"	"#1D42C")
("<b-up-t>"	"#1D42D")
("<b-up-u>"	"#1D42E")
("<b-up-v>"	"#1D42F")
("<b-up-w>"	"#1D430")
("<b-up-x>"	"#1D431")
("<b-up-y>"	"#1D432")
("<b-up-z>"	"#1D433")

;; Bold symbols
("<b-A>"	"#1D468")
("<b-B>"	"#1D469")
("<b-C>"	"#1D46A")
("<b-D>"	"#1D46B")
("<b-E>"	"#1D46C")
("<b-F>"	"#1D46D")
("<b-G>"	"#1D46E")
("<b-H>"	"#1D46F")
("<b-I>"	"#1D470")
("<b-J>"	"#1D471")
("<b-K>"	"#1D472")
("<b-L>"	"#1D473")
("<b-M>"	"#1D474")
("<b-N>"	"#1D475")
("<b-O>"	"#1D476")
("<b-P>"	"#1D477")
("<b-Q>"	"#1D478")
("<b-R>"	"#1D479")
("<b-S>"	"#1D47A")
("<b-T>"	"#1D47B")
("<b-U>"	"#1D47C")
("<b-V>"	"#1D47D")
("<b-W>"	"#1D47E")
("<b-X>"	"#1D47F")
("<b-Y>"	"#1D480")
("<b-Z>"	"#1D481")
("<b-a>"	"#1D482")
("<b-b>"	"#1D483")
("<b-c>"	"#1D484")
("<b-d>"	"#1D485")
("<b-e>"	"#1D486")
("<b-f>"	"#1D487")
("<b-g>"	"#1D488")
("<b-h>"	"#1D489")
("<b-i>"	"#1D48A")
("<b-j>"	"#1D48B")
("<b-k>"	"#1D48C")
("<b-l>"	"#1D48D")
("<b-m>"	"#1D48E")
("<b-n>"	"#1D48F")
("<b-o>"	"#1D490")
("<b-p>"	"#1D491")
("<b-q>"	"#1D492")
("<b-r>"	"#1D493")
("<b-s>"	"#1D494")
("<b-t>"	"#1D495")
("<b-u>"	"#1D496")
("<b-v>"	"#1D497")
("<b-w>"	"#1D498")
("<b-x>"	"#1D499")
("<b-y>"	"#1D49A")
("<b-z>"	"#1D49B")

;; Script symbols
("<cal-A>"	"#1D49C")
;("<cal-B>"	"#212C")
("<cal-C>"	"#1D49E")
("<cal-D>"	"#1D49F")
;("<cal-E>"	"#2130")
;("<cal-F>"	"#2131")
("<cal-G>"	"#1D4A2")
;("<cal-H>"	"#210B")
;("<cal-I>"	"#2110")
("<cal-J>"	"#1D4A5")
("<cal-K>"	"#1D4A6")
;("<cal-L>"	"#2112")
;("<cal-M>"	"#2133")
("<cal-N>"	"#1D4A9")
("<cal-O>"	"#1D4AA")
("<cal-P>"	"#1D4AB")
("<cal-Q>"	"#1D4AC")
;("<cal-R>"	"#211B")
("<cal-S>"	"#1D4AE")
("<cal-T>"	"#1D4AF")
("<cal-U>"	"#1D4B0")
("<cal-V>"	"#1D4B1")
("<cal-W>"	"#1D4B2")
("<cal-X>"	"#1D4B3")
("<cal-Y>"	"#1D4B4")
("<cal-Z>"	"#1D4B5")
("<cal-a>"	"#1D4B6")
("<cal-b>"	"#1D4B7")
("<cal-c>"	"#1D4B8")
("<cal-d>"	"#1D4B9")
;("<cal-e>"	"#212F")
("<cal-f>"	"#1D4BB")
;("<cal-g>"	"#210A")
("<cal-h>"	"#1D4BD")
("<cal-i>"	"#1D4BE")
("<cal-j>"	"#1D4BF")
("<cal-k>"	"#1D4C0")
("<cal-l>"	"#1D4C1")
("<cal-m>"	"#1D4C2")
("<cal-n>"	"#1D4C3")
;("<cal-o>"	"#2134")
("<cal-p>"	"#1D4C5")
("<cal-q>"	"#1D4C6")
("<cal-r>"	"#1D4C7")
("<cal-s>"	"#1D4C8")
("<cal-t>"	"#1D4C9")
("<cal-u>"	"#1D4CA")
("<cal-v>"	"#1D4CB")
("<cal-w>"	"#1D4CC")
("<cal-x>"	"#1D4CD")
("<cal-y>"	"#1D4CE")
("<cal-z>"	"#1D4CF")

;; Bold script symbols
("<b-cal-A>"	"#1D4D0")
("<b-cal-B>"	"#1D4D1")
("<b-cal-C>"	"#1D4D2")
("<b-cal-D>"	"#1D4D3")
("<b-cal-E>"	"#1D4D4")
("<b-cal-F>"	"#1D4D5")
("<b-cal-G>"	"#1D4D6")
("<b-cal-H>"	"#1D4D7")
("<b-cal-I>"	"#1D4D8")
("<b-cal-J>"	"#1D4D9")
("<b-cal-K>"	"#1D4DA")
("<b-cal-L>"	"#1D4DB")
("<b-cal-M>"	"#1D4DC")
("<b-cal-N>"	"#1D4DD")
("<b-cal-O>"	"#1D4DE")
("<b-cal-P>"	"#1D4DF")
("<b-cal-Q>"	"#1D4E0")
("<b-cal-R>"	"#1D4E1")
("<b-cal-S>"	"#1D4E2")
("<b-cal-T>"	"#1D4E3")
("<b-cal-U>"	"#1D4E4")
("<b-cal-V>"	"#1D4E5")
("<b-cal-W>"	"#1D4E6")
("<b-cal-X>"	"#1D4E7")
("<b-cal-Y>"	"#1D4E8")
("<b-cal-Z>"	"#1D4E9")
("<b-cal-a>"	"#1D4EA")
("<b-cal-b>"	"#1D4EB")
("<b-cal-c>"	"#1D4EC")
("<b-cal-d>"	"#1D4ED")
("<b-cal-e>"	"#1D4EE")
("<b-cal-f>"	"#1D4EF")
("<b-cal-g>"	"#1D4F0")
("<b-cal-h>"	"#1D4F1")
("<b-cal-i>"	"#1D4F2")
("<b-cal-j>"	"#1D4F3")
("<b-cal-k>"	"#1D4F4")
("<b-cal-l>"	"#1D4F5")
("<b-cal-m>"	"#1D4F6")
("<b-cal-n>"	"#1D4F7")
("<b-cal-o>"	"#1D4F8")
("<b-cal-p>"	"#1D4F9")
("<b-cal-q>"	"#1D4FA")
("<b-cal-r>"	"#1D4FB")
("<b-cal-s>"	"#1D4FC")
("<b-cal-t>"	"#1D4FD")
("<b-cal-u>"	"#1D4FE")
("<b-cal-v>"	"#1D4FF")
("<b-cal-w>"	"#1D500")
("<b-cal-x>"	"#1D501")
("<b-cal-y>"	"#1D502")
("<b-cal-z>"	"#1D503")

;; Fraktur symbols
("<frak-A>"	"#1D504")
("<frak-B>"	"#1D505")
;("<frak-C>"	"#212D")
("<frak-D>"	"#1D507")
("<frak-E>"	"#1D508")
("<frak-F>"	"#1D509")
("<frak-G>"	"#1D50A")
;("<frak-H>"	"#210C")
;("<frak-I>"	"#2111")
("<frak-J>"	"#1D50D")
("<frak-K>"	"#1D50E")
("<frak-L>"	"#1D50F")
("<frak-M>"	"#1D510")
("<frak-N>"	"#1D511")
("<frak-O>"	"#1D512")
("<frak-P>"	"#1D513")
("<frak-Q>"	"#1D514")
;("<frak-R>"	"#211C")
("<frak-S>"	"#1D516")
("<frak-T>"	"#1D517")
("<frak-U>"	"#1D518")
("<frak-V>"	"#1D519")
("<frak-W>"	"#1D51A")
("<frak-X>"	"#1D51B")
("<frak-Y>"	"#1D51C")
;("<frak-Z>"	"#2128")
("<frak-a>"	"#1D51E")
("<frak-b>"	"#1D51F")
("<frak-c>"	"#1D520")
("<frak-d>"	"#1D521")
("<frak-e>"	"#1D522")
("<frak-f>"	"#1D523")
("<frak-g>"	"#1D524")
("<frak-h>"	"#1D525")
("<frak-i>"	"#1D526")
("<frak-j>"	"#1D527")
("<frak-k>"	"#1D528")
("<frak-l>"	"#1D529")
("<frak-m>"	"#1D52A")
("<frak-n>"	"#1D52B")
("<frak-o>"	"#1D52C")
("<frak-p>"	"#1D52D")
("<frak-q>"	"#1D52E")
("<frak-r>"	"#1D52F")
("<frak-s>"	"#1D530")
("<frak-t>"	"#1D531")
("<frak-u>"	"#1D532")
("<frak-v>"	"#1D533")
("<frak-w>"	"#1D534")
("<frak-x>"	"#1D535")
("<frak-y>"	"#1D536")
("<frak-z>"	"#1D537")

;; Double-struck symbols
("<bbb-A>"	"#1D538")
("<bbb-B>"	"#1D539")
;("<bbb-C>"	"#2102")
("<bbb-D>"	"#1D53B")
("<bbb-E>"	"#1D53C")
("<bbb-F>"	"#1D53D")
("<bbb-G>"	"#1D53E")
;("<bbb-H>"	"#210D")
("<bbb-I>"	"#1D540")
("<bbb-J>"	"#1D541")
("<bbb-K>"	"#1D542")
("<bbb-L>"	"#1D543")
("<bbb-M>"	"#1D544")
;("<bbb-N>"	"#2115")
("<bbb-O>"	"#1D546")
;("<bbb-P>"	"#2119")
;("<bbb-Q>"	"#211A")
;("<bbb-R>"	"#211D")
("<bbb-S>"	"#1D54A")
("<bbb-T>"	"#1D54B")
("<bbb-U>"	"#1D54C")
("<bbb-V>"	"#1D54D")
("<bbb-W>"	"#1D54E")
("<bbb-X>"	"#1D54F")
("<bbb-Y>"	"#1D550")
;("<bbb-Z>"	"#2124")
("<bbb-a>"	"#1D552")
("<bbb-b>"	"#1D553")
("<bbb-c>"	"#1D554")
("<bbb-d>"	"#1D555")
("<bbb-e>"	"#1D556")
("<bbb-f>"	"#1D557")
("<bbb-g>"	"#1D558")
("<bbb-h>"	"#1D559")
("<bbb-i>"	"#1D55A")
("<bbb-j>"	"#1D55B")
("<bbb-k>"	"#1D55C")
("<bbb-l>"	"#1D55D")
("<bbb-m>"	"#1D55E")
("<bbb-n>"	"#1D55F")
("<bbb-o>"	"#1D560")
("<bbb-p>"	"#1D561")
("<bbb-q>"	"#1D562")
("<bbb-r>"	"#1D563")
("<bbb-s>"	"#1D564")
("<bbb-t>"	"#1D565")
("<bbb-u>"	"#1D566")
("<bbb-v>"	"#1D567")
("<bbb-w>"	"#1D568")
("<bbb-x>"	"#1D569")
("<bbb-y>"	"#1D56A")
("<bbb-z>"	"#1D56B")

;; Bold Greek uppercase symbols
("<b-Alpha>"	"#1D6A8")
("<b-Beta>"	"#1D6A9")
("<b-Gamma>"	"#1D6AA")
("<b-Delta>"	"#1D6AB")
("<b-Epsilon>"	"#1D6AC")
("<b-Zeta>"	"#1D6AD")
("<b-Eta>"	"#1D6AE")
("<b-Theta>"	"#1D6AF")
("<b-Iota>"	"#1D6B0")
("<b-Kappa>"	"#1D6B1")
("<b-Lambda>"	"#1D6B2")
("<b-Mu>"	"#1D6B3")
("<b-Nu>"	"#1D6B4")
("<b-Xi>"	"#1D6B5")
("<b-Omicron>"	"#1D6B6")
("<b-Pi>"	"#1D6B7")
("<b-Rho>"	"#1D6B8")
("<b-Sigma>"	"#1D6BA")
("<b-Tau>"	"#1D6BB")
("<b-Upsilon>"	"#1D6BC")
("<b-Phi>"	"#1D6BD")
("<b-Chi>"	"#1D6BE")
("<b-Psi>"	"#1D6BF")
("<b-Omega>"	"#1D6C0")

;; Bold Greek lowercase symbols
("<b-up-alpha>"	     "#1D6C2")
("<b-up-beta>"	     "#1D6C3")
("<b-up-gamma>"	     "#1D6C4")
("<b-up-delta>"	     "#1D6C5")
("<b-up-varepsilon>" "#1D6C6")
("<b-up-zeta>"	     "#1D6C7")
("<b-up-eta>"	     "#1D6C8")
("<b-up-theta>"	     "#1D6C9")
("<b-up-iota>"	     "#1D6CA")
("<b-up-kappa>"	     "#1D6CB")
("<b-up-lambda>"     "#1D6CC")
("<b-up-mu>"	     "#1D6CD")
("<b-up-nu>"	     "#1D6CE")
("<b-up-xi>"	     "#1D6CF")
("<b-up-omicron>"    "#1D6D0")
("<b-up-pi>"	     "#1D6D1")
("<b-up-rho>"	     "#1D6D2")
("<b-up-varsigma>"   "#1D6D3")
("<b-up-sigma>"	     "#1D6D4")
("<b-up-tau>"	     "#1D6D5")
("<b-up-upsilon>"    "#1D6D6")
("<b-up-varphi>"     "#1D6D7")
("<b-up-chi>"	     "#1D6D8")
("<b-up-psi>"	     "#1D6D9")
("<b-up-omega>"	     "#1D6DA")

;; Additional bold Greek symbols
("<b-up-epsilon>"  "#1D6DC")
("<b-up-vartheta>" "#1D6DD")
("<b-up-varkappa>" "#1D6DE")
("<b-up-phi>"	   "#1D6DF")
("<b-up-varrho>"   "#1D6E0")
("<b-up-varpi>"	   "#1D6E1")

;; Bold Italic Greek lowercase symbols
("<b-alpha>"	  "#1D736")
("<b-beta>"	  "#1D737")
("<b-gamma>"	  "#1D738")
("<b-delta>"	  "#1D739")
("<b-varepsilon>" "#1D73A")
("<b-zeta>"	  "#1D73B")
("<b-eta>"	  "#1D73C")
("<b-theta>"	  "#1D73D")
("<b-iota>"	  "#1D73E")
("<b-kappa>"	  "#1D73F")
("<b-lambda>"	  "#1D740")
("<b-mu>"	  "#1D741")
("<b-nu>"	  "#1D742")
("<b-xi>"	  "#1D743")
("<b-omicron>"	  "#1D744")
("<b-pi>"	  "#1D745")
("<b-rho>"	  "#1D746")
("<b-varsigma>"	  "#1D747")
("<b-sigma>"	  "#1D748")
("<b-tau>"	  "#1D749")
("<b-upsilon>"	  "#1D74A")
("<b-varphi>"	  "#1D74B")
("<b-chi>"	  "#1D74C")
("<b-psi>"	  "#1D74D")
("<b-omega>"	  "#1D74E")

;; Additional Bold Italic Greek symbols
("<b-epsilon>"  "#1D750")
("<b-vartheta>"	"#1D751")
("<b-varkappa>"	"#1D752")
("<b-phi>"	"#1D753")
("<b-varrho>"	"#1D754")
("<b-varpi>"	"#1D755")

;; Bold digits
("<b-0>"	"#1D7CE")
("<b-1>"	"#1D7CF")
("<b-2>"	"#1D7D0")
("<b-3>"	"#1D7D1")
("<b-4>"	"#1D7D2")
("<b-5>"	"#1D7D3")
("<b-6>"	"#1D7D4")
("<b-7>"	"#1D7D5")
("<b-8>"	"#1D7D6")
("<b-9>"	"#1D7D7")

;;; Musical Symbols					1D100--1D1FF
("<fullnote>"	"#1D15D")
("<halfnote>"	"#1D15E")
