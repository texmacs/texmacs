;; Conversion between TeXmacs universal symbols and Unicode

;; (C) 2002-2003  Felix Breuer, David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details. If
;; you don't have this file, write to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Symbols are sorted by Unicode value and grouped by Unicode block.
;;
;; When the codepage grouping conflict with the logical grouping (e.g.
;; Mathematical Alphanumeric Symbols which are present in the Letterlike
;; Symbols block), the Unicode ordering is preserved but the translation pair
;; is also placed in a comment in the logical group.
;;
;; The intent is making it easy to refer to the Unicode charts, and ensure that
;; logical groups are complete and correct.


;;; Symbols which do not seem to exist in Unicode

;; No appropriate "sans-serif capital y"
;("<Ydown>"	"")
;("<Yleft>"	"")
;("<Yright>"	"")

;; No hook character (only as combining char and combinations)
;("<lefthook>"	"")
;("<righthook>"	"")

;; No appropriate "arrow with hook"
;("<longhookrightarrow>"	"")
;("<hookuparrow>"		"")
;("<hookdownarrow>"		"")

;; No "open-headed arrow from bar"
;("<mapstotriangle>"		"")
;("<longmapstotriangle>"	"")

;; No "long upwards arrow" or "long downwards arrow"
;("<longtwoheadleftarrow>"	"")
;("<longtwoheadrightarrow>"	"")
;("<longuparrow>"		"")
;("<longdownarrow>"		"")
;("<longupdownarrow>"		"")
;("<Longuparrow>"		"")
;("<Longdownarrow>"		"")
;("<Longupdownarrow>"		"")
;("<longmapsup>"		"")
;("<longmapsdown>"		"")
;("<longhookuparrow>"		"")
;("<longhookdownarrow>"		"")

;; Not possible to express the semantic difference from regular arrows
;("<leftarrowlim>"		"")
;("<rightarrowlim>"		"")
;("<leftrightarrowlim>"		"")
;("<mapstolim>"			"")
;("<longleftarrowlim>"		"")
;("<longrightarrowlim>"		"")
;("<longleftrightarrowlim>"	"")
;("<longmapstolim>"		"")

;; No appropriate bold greek symbol
;("<b-Varupsilon>"	"")
;("<b-backepsilon>"	"")

;; No component for horizontal curly brackets
;("<braceld>"	"")
;("<bracerd>"	"")
;("<bracelu>"	"")
;("<braceru>"	"")

;; No dotless j
;("<jmath>"	"")
;("<j*>"	"")

;; Unclear translation, preferring imath
;("<i*>"	"") ; see <imath>

;; No unary variants
;("<um>"	"") ; see <minus>
;("<upm>"	"") ; see <pm>

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

;; Large symbols which have an _approximative_ unicode translation
;("<wilde-tilde>"	"#02DC") ; small tilde
;("<wide-hat>"		"#02C6") ; modifier letter circumflex accent

;("<large-/-0>"		"/")
;("<large-\\-0"		"\\")
;("<large-less-0>"	"<")
;("<large-gtr-0>"	">")
;("<large-(-0>"		"(")
;("<large-)-0>"		")")
;("<large-[-0>"		"[")
;("<large-]-0>"		"]")
;("<large-lceil-0>"	"#2308")
;("<large-lfloor-0>"	"#2309")
;("<large-lfloor-0>"	"#230A")
;("<large-rfloor-0>"	"#230B")
;("<large-{-0>"		"{")
;("<large-}-0>"		"}")
;("<large-|-0>"		"|")
;("<large-||-0>"		"")

;; The conversion of the small symbol is probematic
;; MathML uses a symbol which unicode discourage to use in math
;("<large-langle-0>"	"")
;("<large-rangle-0>"	"")

;; Other problematic symbols

;; is it a variant of <leftsquigarrow> or the actual "leftwards wave arrow"?
;("<wasyleadsto>"	"#219C")
;; is it a synonym for "\" or the "reverse solidus operator"
;; (texmacs.syx defines it as a symbol)...
;("<backslash>"		"#29F5") ; U+29F5 is "reverse solidus operator"
;; typo in the name of the symbol... should be <nshortmid>
("<nshormid>"		"#2224") ; variant of <nmid>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Control and Basic Latin			0000--007F

("<less>"	"#3C")	; overrides corktounicode
("<gtr>"	"#3E")	; overrides corktounicode


;;; Controls and Latin-1 Supplement		0080--00FF

; 0080--009F are control chars (several of would be useful in texmacs)
; no break space "#A0"
;("<flip-!>"	"#A1") ; see symbol-unicode-math.scm
("<cent>"	"#A2")
; pound sign	"#A3"
; currency sign	"#A4"
("<yen>"	"#A5")
("<brokenvert>"	"#A6")
;("<paragraph>"	"#A7") ; see symbol-unicode-math.scm
;("<ddot>"	"#A8") ; see symbol-unicode-math.scm
("<copyright>"	"#A9")
; feminine ordinal indicator "#AA"
; left pointing double angle quotation mark "#AB"
("<neg>"	"#AC")
; soft hyphen	"#AD"
; registered sign "#AE"	; not circled R
;("<bar>"	"#AF") ; see symbol-unicode-math.scm
; degree sign	"#B0"
("<pm>"		"#B1")
; superscript two "#B2"
; superscript three "#B3"
;("<acute>"	"#B4") ; see symbol-unicode-math.scm
; micro sign	"#B5"
("<endofline>"	"#B6")
; middle dot	"#B7"  ; not <cdot>
;("<cedille>"	"#B8") ; see symbol-unicode-math.scm
; superscript one "#B9"
; masculine ordinal indicator "#BA"
; right pointing double angle quotation mark "#BB"
; vulgar fraction one quarter "#BC"
; vulgar fraction one half "#BD"
; vulgar fraction three quarters "#BE"
;("<flip-?>"	"#BF") ; see symbol-unicode-math.scm
;("<AE>"	"#C6") ; see symbol-unicode-math.scm
;("<Thorn>"	"#DE") ; see symbol-unicode-math.scm
("<times>"	"#D7")
;("<O/>"	"#D8") ; see symbol-unicode-math.scm
;("<sz>"	"#DF") ; see symbol-unicode-math.scm
;("<ae>"	"#E6") ; see symbol-unicode-math.scm
;("<dh>"	"#F0") ; see symbol-unicode-math.scm
("<div>"	"#F7")
;("<thorn>"	"#FE") ; see symbol-unicode-math.scm


;;; Latin Extended-A				0100--017F

; h with stroke "#0127"  ; not <hbar> (plank constant)
("<imath>"	"#0131")
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
("<||>"		"#2016")
;("<``>"	"#201C") ; see symbol-unicode-math.scm
("<dagger>"	"#2020")
("<ddagger>"	"#2021")
("<bullet>"	"#2022")

;; General punctuation
;("<permil>"	"#2030") ; see symbol-unicode-math.scm
("<prime>"	"#2032")
("<backprime>"	"#2035")
("<ldots>"	"#2026")


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
; <cal-g>	"#210A"
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
; <cal-e>	"#212F"
("<cal-E>"	"#2130")
("<cal-F>"	"#2131")
; turned capital f "#2132"
("<cal-M>"	"#2133")
; <cal-o>	"#2134"

;; Hebrew letterlike math symbols
("<aleph>"	"#2135")
("<beth>"	"#2136")
("<gimel>"	"#2137")
("<daleth>"	"#2138")

;; Additional letterlike symbols
; information source	"#2139"
; rotated capital q	"#213A"
; facsimile sign	"#213B"
; (reserved)		"#213C"
; double-struck small gamma "#213D"
; double-struck capital gamma "#213E"
; double-struck capital pi "#213F"

;; Double-struck large operator
; double-struck n-ary summation "#2140" 

;; Additional letterlike symbols
("<Game>"	"#2141") ; example glyph is turned 180°, not flipped
; turned sans-serif capital l "#2142"
; reversed sans-serif capital l "#2143"
("<Yup>"	"#2144")

;; Double-struck italic mathematical symbols
;; NOTE: TeXmacs represent those as "straigh" (aka. roman) symbols
("<mathD>"	"#2145")
("<mathd>"	"#2146")
("<mathe>"	"#2147")
("<mathi>"	"#2148")
; <mathj>	"#2149"

;; Additional letterlike symbols
; property line	"#214A"
("<bindnasrepma>" "#214B")


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
("<rightsquigarrow>" 	"#219C") ; nominally "rightwards wave arrow"
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

;; Long arrows

;; According to Unicode: "The long arrows are used for mapping whereas
;; the short forms would be used in limits.
;("<longleftarrow>"	"#27F5")
;("<longrightarrow>"	"#27F6")
;("<longleftrightarrow>"	"#27F7")
;("<Longleftarrow>"	"#27F8")
;("<Longrightarrow>"	"#27F9")
;("<Longleftrightarrow>"	"#27FA")
;("<longmapsfrom>"	"#27FB")
;("<longmapsto>"		"#27FC")


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
; small contains as member "#220D"
; end of proof		"#220E"
("<prod>"		"#220F")
("<amalg>"		"#2210")
("<sum>"		"#2211")
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
("<int>"		"#222B")
; double integral	"#222E"
; triple integral	"#222D"
; <oint>		"#222E"
; surface integral	"#222F"
; volume integral	"#2230"
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
; <doteq>		"#2250"
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
; FIXME: rename circledminus to varodash
; FIXME: define symbol odash with correct circle line width
("<circledminus>"	"#229D") ; FIXME: use odash instead
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
("<barwedge>"		"#22BC")
; <barvee>		"#22BD"
; right angle with arc	"#22BE"
; right triangle	"#22BF"
; <big-wedge>		"#22C0"  ; does not render
; <big-vee>		"#22C1"  ; does not render
; <big-cap>		"#22C2"  ; does not render
; <bigcup>"		"#22C3"  ; does not render
("<diamond>"		"#22C4") ; TODO: define as arithmetic-times
("<cdot>"		"#22C5") ; not MIDDLE DOT
("<star>"		"#22C6")
("<divideontimes>"	"#22C7")
("<bowtie>"		"#22C8") ; not <join>
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
("<gtrlesseq>"		"#22DB")
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
("<ntriangleqleft>"	"#22EC")
("<ntriangleqright>"	"#22ED")
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
; projective		"#2305"  ; not <barwedge> which is NAND
("<doublebarwedge>"	"#2306") ; perspective
; vawy line		"#2307"

;; Corner brackets
("<lceil>"		"#2308")
("<rceil>"		"#2309")
("<lfloor>"		"#230A")
("<rfloor>"		"#230B")

("<invneg>"		"#2310")
("<frown>"		"#2322")
("<smile>"		"#2323")


;;; Geometric Shapes				25A0--25FF

;("<blacksquare>"	"#25A0")
;("<box>"		"#25A1")
;("<triangleright>"	"#25B9")
;("<triangleleft>"	"#25C3")

;;; Miscellaneous Symbols			2600--26FF

;("<phone>"		"#260E")
;("<female>"		"#2640")
;("<earth>"		"#2641")
;("<male>"		"#2642")
;("<spadesuit>"		"#2660")
;("<heartsuit>"		"#2661")
;("<diamondsuit>"	"#2662")
;("<clubsuit>"		"#2663")
;("<quarternote>"	"#2669")
;("<eighthnote>"		"#266A")
;("<twonotes>"		"#266B")
;("<flat>"		"#266D")
;("<natural>"		"#266E")
;("<sharp>"		"#266F")

;;; Miscellaneous Mathematical Symbols-A	27C0--21FF

;("<llbracket>"		"#27E6")
;("<rrbracket>"		"#27E7")
;("<langle>"		"#27E8")
;("<rangle>"		"#27E9")

;;; Supplemental Mathematical Operators		2A00-2AFF

("<leqslant>"		"#2A7D")
("<geqslant>"		"#2A7E")
("<lessapprox>"		"#2A85")
("<gtrapprox>"		"#2A86")
("<lneq>"		"#2A87")
("<gneq>"		"#2A88")
("<lesseqqgtr>"		"#2A8B")
("<gtreqqless>"		"#2A8C")
("<eqslantless>"	"#2A95")
("<eqslantgtr>"		"#2A96")
("<preceq>"		"#2AAF")
("<succeq>"		"#2AB0")
("<precapprox>"		"#2AB7")
("<succapprox>"		"#2AB8")
("<precnapprox>"	"#2AB9")
("<succnapprox>"	"#2ABA")
("<subseteqq>"		"#2AC5")
("<supseteqq>"		"#2AC6")
("<subsetneqq>"		"#2ACB")
("<supsetneqq>"		"#2ACC")

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
("<vartheta>"	"#03D1")
("<phi>"	"#03D5")
("<varpi>"	"#03D6")
("<digamma>"	"#03DC")
("<varkappa>"	"#03F0")
("<varrho>"	"#03F1")
("<epsilon>"	"#03F5")
("<backepsilon>" "#03F6")

;;; Halfwidth and Fullwidth Forms		FF00--FFEF

("<longminus>"	"#FF0D")
("<longequal>"	"#FF1D")

;;; Mathematical Alphanumeric Symbols		1D400--1D7FF

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

;; TeXmacs has no lowercase script symbol
;("<cal-a>"	"#1D4B6")
;("<cal-b>"	"#1D4B7")
;("<cal-c>"	"#1D4B8")
;("<cal-d>"	"#1D4B9")
;;("<cal-e>"	"#212F")
;("<cal-f>"	"#1D4BB")
;;("<cal-g>"	"#210A")
;("<cal-h>"	"#1D4BD")
;("<cal-i>"	"#1D4BE")
;("<cal-j>"	"#1D4BF")
;("<cal-k>"	"#1D4C0")
;("<cal-l>"	"#1D4C1")
;("<cal-m>"	"#1D4C2")
;("<cal-n>"	"#1D4C3")
;;("<cal-o>"	"#2134")
;("<cal-p>"	"#1D4C5")
;("<cal-q>"	"#1D4C6")
;("<cal-r>"	"#1D4C7")
;("<cal-s>"	"#1D4C8")
;("<cal-t>"	"#1D4C9")
;("<cal-u>"	"#1D4CA")
;("<cal-v>"	"#1D4CB")
;("<cal-w>"	"#1D4CC")
;("<cal-x>"	"#1D4CD")
;("<cal-y>"	"#1D4CE")
;("<cal-z>"	"#1D4CF")

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

;; TeXmacs has no lowercase script symbol
;("<b-cal-a>"	"#1D4EA")
;("<b-cal-b>"	"#1D4EB")
;("<b-cal-c>"	"#1D4EC")
;("<b-cal-d>"	"#1D4ED")
;("<b-cal-e>"	"#1D4EE")
;("<b-cal-f>"	"#1D4EF")
;("<b-cal-g>"	"#1D4F0")
;("<b-cal-h>"	"#1D4F1")
;("<b-cal-i>"	"#1D4F2")
;("<b-cal-j>"	"#1D4F3")
;("<b-cal-k>"	"#1D4F4")
;("<b-cal-l>"	"#1D4F5")
;("<b-cal-m>"	"#1D4F6")
;("<b-cal-n>"	"#1D4F7")
;("<b-cal-o>"	"#1D4F8")
;("<b-cal-p>"	"#1D4F9")
;("<b-cal-q>"	"#1D4FA")
;("<b-cal-r>"	"#1D4FB")
;("<b-cal-s>"	"#1D4FC")
;("<b-cal-t>"	"#1D4FD")
;("<b-cal-u>"	"#1D4FE")
;("<b-cal-v>"	"#1D4FF")
;("<b-cal-w>"	"#1D500")
;("<b-cal-x>"	"#1D501")
;("<b-cal-y>"	"#1D502")
;("<b-cal-z>"	"#1D503")

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
("<b-alpha>"	"#1D6C2")
("<b-beta>"	"#1D6C3")
("<b-gamma>"	"#1D6C4")
("<b-delta>"	"#1D6C5")
("<b-varepsilon>" "#1D6C6")
("<b-zeta>"	"#1D6C7")
("<b-eta>"	"#1D6C8")
("<b-theta>"	"#1D6C9")
("<b-iota>"	"#1D6CA")
("<b-kappa>"	"#1D6CB")
("<b-lambda>"	"#1D6CC")
("<b-mu>"	"#1D6CD")
("<b-nu>"	"#1D6CE")
("<b-xi>"	"#1D6CF")
("<b-omicron>"	"#1D6D0")
("<b-pi>"	"#1D6D1")
("<b-rho>"	"#1D6D2")
("<b-varsigma>"	"#1D6D3")
("<b-sigma>"	"#1D6D4")
("<b-tau>"	"#1D6D5")
("<b-upsilon>"	"#1D6D6")
("<b-varphi>"	"#1D6D7")
("<b-chi>"	"#1D6D8")
("<b-psi>"	"#1D6D9")
("<b-omega>"	"#1D6DA")

;; Additional bold Greek symbols
("<b-epsilon>"	"#1D6DC")
("<b-vartheta>"	"#1D6DD")
("<b-varkappa>"	"#1D6DE")
("<b-phi>"	"#1D6DF")
("<b-varrho>"	"#1D6E0")
("<b-varpi>"	"#1D6E1")

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
