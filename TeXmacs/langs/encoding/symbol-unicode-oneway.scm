;; One-way conversion of some TeXmacs symbols to Unicode.

;; (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

;; Those symbols have a sensible unicode translation, so they can be exported.
;; But another symbol was chosen for the importation of the corresponding
;; unicode character. So the exportation is one-way only.

;;; Synonyms for other symbols

;; Mathematical constants are displayed upright in TeXmacs
;; Admit variants <mathD*>, <mathd*>, etc.

("<mathcatalan>"   "#43")   ; Catalan constant
("<mathD>"	   "#44")   ; Derivation operator
("<mathd>"	   "#64")   ; differential (for d x inside integrals)
("<mathe>"	   "#65")   ; exp(1)
("<mathi>"	   "#69")   ; imaginary unit
("<mathj>"	   "#6A")   ; variant of imaginary unit
("<mathGamma>"	   "#0393") ; Gamma function
("<mathLaplace>"   "#0394") ; Laplace operator
("<mathpi>"	   "#03C0") ; pi constant
("<matheuler>"	   "#03B3") ; Euler constant
("<mathlambda>"	   "#03BB") ; lambda calculus "quantifier"

;; Alternate symbol names. They use the same glyph and are converted to the
;; same unicode character as another symbol.

("<dag>"	"#2020") ; see <dagger>
("<ddag>"	"#2021") ; see <ddagger>
("<Im>"		"#2111") ; see <frak-I>
("<Re>"		"#211C") ; see <frak-R>
("<agemO>"	"#2127") ; see <Mho>
("<to>"		"#2192") ; see <rightarrow>
("<notin>"	"#2209") ; see <nin>
("<notni>"	"#220C") ; see <nni>
("<asterisk>"	"#2217") ; see <ast>
("<le>"		"#2264") ; see <leq>
("<ge>"		"#2265") ; see <geq>
("<perp>"	"#22A5") ; see <bot>
("<lhd>"	"#22B2") ; see <vartriangleleft>
("<wasylhd>"	"#22B2") ; see <vartriangleleft>
("<rhd>"	"#22B3") ; see <vartriangleright>
("<wasyrhd>"	"#22B3") ; see <vartriangleright>
("<unlhd>"	"#22B4") ; see <trianglelefteq>
("<wasyunlhd>"	"#22B4") ; see <trianglelefteq>
("<unrhd>"	"#22B5") ; see <trianglerighteq>
("<wasyunrhd>"	"#22B5") ; see <trianglerighteq>
("<cdummy>"	"#22C5") ; see <cdot>
("<exterior>"	"#2227") ; see <wedge>
("<box>"	"#25A1") ; see <square>
("<Box>"	"#25A1") ; see <square>
("<wasy-38>"	"#2641") ; see <earth>
("<fullmoon>"	"#25CB") ; see <Circle>
("<newmoon>"	"#25CF") ; see <CIRCLE>

;; Almost synonyms.

;; TeXmacs makes a difference in the baseline position, but Unicode
;; does not have separate characters. The chosen main symbols where
;; those which where closest to the baseline.

("<venus>"	"#2640") ; see <female>
("<mars>"	"#2642") ; see <male>

;;; Glyph variants

;; Symbols for alternate glyphs. They are converted to the same unicode
;; character as another symbol but are associated to a slightly different
;; glyph.

;; They might be exported conservatively using "Variant Selectors" (FE00--FE0F)
;; combining characters. See the MathML rec (chap. 6) for allowed combinations.

("<binampersand>"	"#0026") ; variant of "&", same look as <bindnasrepma>
("<i*>"			"#0131") ; variant (straight) of <imath>
("<hbar>"		"#210F") ; variant of <hslash>
("<varlightning>"	"#21AF") ; variant of <lightning>
("<varnothing>"		"#2205") ; variant of <emptyset>
("<smallsetminus>"	"#2216") ; variant of <setminus>
("<shortmid>"		"#2223") ; variant of <mid>
("<shortparallel>"	"#2225") ; variant of <parallel>
("<nshortparallel>"	"#2226") ; variant of <nparallel>
("<wasytherefore>"	"#2234") ; variant of <therefore>
("<oplus>"		"#2295") ; variant of <varoplus>
("<ominus>"		"#2296") ; variant of <varominus>
("<otimes>"		"#2297") ; variant of <varotimes>
("<oslash>"		"#2298") ; variant of <varoslash>
("<odot>"		"#2299") ; variant of <varodot>
("<circledcirc>"	"#229A") ; variant of <varocircle>
("<circledast>"		"#229B") ; variant of <varoast>
("<wasysqsubset>"	"#228F") ; variant of <sqsubset>
("<wasysqsupset>"	"#2290") ; variant of <sqsupset>
("<LEFTarrow>"		"#25C2") ; variant of <blacktriangleleft>
("<RIGHTarrow>"		"#25B8") ; variant of <blacktriangleright>
("<UParrow>"		"#25B4") ; variant of <blacktriangleup>
("<DOWNarrow>"		"#25BE") ; variant of <blacktriangledown>
("<bigcirc>"		"#25EF") ; variant of <varbigcirc> (slightly oval)
("<varangle>"		"#2222") ; variant of <sphericalangle>
("<Bbbk>"		"#1D55C") ; variant of <bb-k>
("<triangle>"	        "#25B3") ; variant of <bigtriangleup>
("<nshortmid>"		"#2224") ; variant of <nmid>
("<of>"		        ":")     ; variant of : for "of type"
("<suchthat>"		":")     ; variant of : for { a in ZZ : a > 5 }
("<varsuchthat>"	"|")     ; variant of | for { a in ZZ | a > 5 }

;;; Big delimiters

;; These symbols should only be used internally by TeXmacs in the
;; LEFT, RIGHT, MID and BIG primitive. If they are explicitly
;; present, they are exported as a regular-sized character.	

("<lvert>"		"|")
("<rvert>"		"|")
("<lVert>"		"#2016")
("<rVert>"		"#2016")

("<large-less-0>"	"<")
("<large-gtr-0>"	">")
("<large-(>"		"(")
("<large-)>"		")")
("<large-(-0>"		"(")
("<large-)-0>"		")")
("<large-[>"		"[")
("<large-]>"		"]")
("<large-[-0>"		"[")
("<large-]-0>"		"]")
("<large-lceil>"	"#2308")
("<large-rceil>"	"#2309")
("<large-lfloor>"	"#230A")
("<large-rfloor>"	"#230B")
("<large-lceil-0>"	"#2308")
("<large-rceil-0>"	"#2309")
("<large-lfloor-0>"	"#230A")
("<large-rfloor-0>"	"#230B")
("<large-{>"		"{")
("<large-}>"		"}")
("<large-{-0>"		"{")
("<large-}-0>"		"}")
("<large-|>"		"|")
("<large-lvert>"	"|")
("<large-rvert>"	"|")
("<large-||>"		"#2016")
("<large-lVert>"	"#2016")
("<large-rVert>"	"#2016")
("<large-|-0>"		"|")
("<large-lvert-0>"	"|")
("<large-rvert-0>"	"|")
("<large-||-0>"		"#2016")
("<large-lVert-0>"	"#2016")
("<large-rVert-0>"	"#2016")
("<large-llbracket>"	"#27E6")
("<large-rrbracket>"	"#27E7")
("<large-llbracket-0>"	"#27E6")
("<large-rrbracket-0>"	"#27E7")
("<large-langle>"	"#27E8")
("<large-rangle>"	"#27E9")
("<large-langle-0>"	"#27E8")
("<large-rangle-0>"	"#27E9")

("<large-sqrt-0>"	"#221A") ; FIXME: wrong baseline

;;; Big Operators

;; These symbols should only be use internally by TeXmacs in BIG
;; primitives or other special constructs. If they are explicitely
;; present, they are exported as a regular-sized character.


("<prod>"		"#220F")
("<sum>"		"#2211")
("<int>"		"#222B")
("<iint>"		"#222C")
("<iiint>"		"#222D")
("<iiiint>"		"#2A0C")
("<oint>"		"#222E")
("<oiint>"		"#222F")
("<oiiint>"		"#2230")
("<big-box>"  	        "#25FD")
("<big-square>"         "#25FD")
("<pluscup>"	        "#2A04")
("<big-prod-1>"		"#220F")
("<big-amalg-1>"        "#2210")
("<big-sum-1>"		"#2211")
("<big-int-1>"		"#222B")
("<big-iint-1>"		"#222C")
("<big-iiint-1>"	"#222D")
("<big-iiiint-1>"	"#2A0C")
("<big-oint-1>"		"#222E")
("<big-oiint-1>"	"#222F")
("<big-oiiint-1>"	"#2230")
("<big-wedge-1>"        "#22C0")
("<big-vee-1>"		"#22C1")
("<big-cap-1>"		"#22C2")
("<big-cup-1>"		"#22C3")
("<big-box-1>"	        "#25FD")
("<big-square-1>"       "#25FD")
("<big-odot-1>"		"#2A00")
("<big-oplus-1>"	"#2A01")
("<big-otimes-1>"	"#2A02")
("<big-pluscup-1>"	"#2A04")
("<big-sqcap-1>"   	"#2A05")
("<big-sqcup-1>"  	"#2A06")

("<upint>"		"#222B")
("<upiint>"		"#222C")
("<upiiint>"		"#222D")
("<upiiiint>"		"#2A0C")
("<upoint>"		"#222E")
("<upoiint>"		"#222F")
("<upoiiint>"		"#2230")
("<big-upint-1>"	"#222B")
("<big-upiint-1>"	"#222C")
("<big-upiiint-1>"	"#222D")
("<big-upiiiint-1>"	"#2A0C")
("<big-upoint-1>"	"#222E")
("<big-upoiint-1>"	"#222F")
("<big-upoiiint-1>"	"#2230")
("<intlim>"		"#222B")
("<iintlim>"		"#222C")
("<iiintlim>"		"#222D")
("<iiiintlim>"		"#2A0C")
("<ointlim>"		"#222E")
("<oiintlim>"		"#222F")
("<oiiintlim>"		"#2230")
("<big-intlim-1>"	"#222B")
("<big-iintlim-1>"	"#222C")
("<big-iiintlim-1>"	"#222D")
("<big-iiiintlim-1>"	"#2A0C")
("<big-ointlim-1>"	"#222E")
("<big-oiintlim-1>"	"#222F")
("<big-oiiintlim-1>"	"#2230")
("<upintlim>"		"#222B")
("<upiintlim>"		"#222C")
("<upiiintlim>"		"#222D")
("<upiiiintlim>"	"#2A0C")
("<upointlim>"		"#222E")
("<upoiintlim>"		"#222F")
("<upoiiintlim>"	"#2230")
("<big-upintlim-1>"	"#222B")
("<big-upiintlim-1>"	"#222C")
("<big-upiiintlim-1>"	"#222D")
("<big-upiiiint-1>"	"#2A0C")
("<big-upointlim-1>"	"#222E")
("<big-upoiintlim-1>"	"#222F")
("<big-upoiiintlim-1>"	"#2230")

;;; Limit symbols

;; These symbols are displayed in the same as their base variant, but TeXmacs
;; displays right subscripts _under_ the symbol instead of at the lower right.
;; The difference with the base symbols must be expressed by markup.

("<leftarrowlim>"		"#2190")
("<rightarrowlim>"		"#2192")
("<leftrightarrowlim>"		"#2194")
("<mapstolim>"			"#21A6")
("<longleftarrowlim>"		"#27F5")
("<longrightarrowlim>"		"#27F6")
("<longleftrightarrowlim>"	"#27F7")
("<longmapstolim>"		"#27FC")
("<Leftarrowlim>"		"#21D0")
("<Leftrightarrowlim>"		"#21D4")
("<Longleftarrowlim>"		"#27F8")
("<Longleftrightarrowlim>"	"#27FA")
("<Longrightarrowlim>"		"#27F9")
("<Rightarrowlim>"		"#21D2")
("<equallim>"			"=")
("<longequallim>"		"#FF1D")

;;; Unary operator variants

;; Unicode makes no distinction between unary and binary operators. TeXmacs
;; unary operator variants are exported as binary operators. The import filter
;; should take care of using unary operators where appropriate.

("<upm>"	"#B1")   ; see <pm>
("<upl>"	"#2B")   ; see +
("<um>"		"#2212") ; see <minus>
("<ump>"	"#2213") ; see <mp>

;;; Extra Mathematical Alphanumeric Symbols

;; Unicode has no character for those symbols. Their font style must
;; be exported using markup and the import filter should import the
;; marked-up base symbol as the correct styled symbol.

("<b-backepsilon>"	"#03F6") ; see <backepsilon>
("<b-ell>"		"#2113") ; see <ell>
("<b-Mho>"		"#2127") ; see <Mho>

;;; Extra invisible symbols

;; Unicode lacks supports for invisible brackets and invisible symbols.
;; For the moment, we export them as "zero width space"

("<nobracket>"    "#200B")
("<nosymbol>"     "#200B")
