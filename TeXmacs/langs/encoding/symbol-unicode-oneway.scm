;; One-way conversion of some TeXmacs symbols to Unicode.

;; (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details. If
;; you don't have this file, write to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Those symbols have a sensible unicode translation, so they can be exported.
;; But another symbol was chosen for the importation of the corresponding
;; unicode character. So the exportation is one-way only.

;;; Synonym for other symbols

;; Alternate symbol names. There use the same glyph and are converted to the
;; same unicode character as another symbol.

("<dag>"	"#2020") ; see <dagger>
("<ddag>"	"#2021") ; see <ddagger>
("<Im>"		"#2111") ; see <frak-I>
("<Re>"		"#211C") ; see <frak-R>
("<agemO>"	"#2127") ; see <Mho>
("<to>"		"#2192") ; see <rigtharrow>
("<asterisk>"	"#2217") ; see <ast>
("<le>"		"#2264") ; see <leq>
("<ge>"		"#2265") ; see <geq>
("<perp>"	"#22A5") ; see <bot>
("<lhd>"	"#22B2") ; see <vartriangleleft>
("<rhd>"	"#22B3") ; see <vartriangleright>
("<unlhd>"	"#22B4") ; see <trianglelefteq>
("<unrhd>"	"#22B5") ; see <trianglerighteq>


;;; Glyph variants

;; Symbols for alternate glyphs. They are converted to the same unicode
;; character as another symbol but are associated to a slightly different
;; glyph.

;; They might be exported conservatively using "Variant Selectors" (FE00--FE0F)
;; combining characters. See the MathML rec (chap. 6) for allowed combinations.

("<binampersand>"	"#0026") ; variant of "&", same look as <bindnasrepma>
("<hbar>"		"#210F") ; variant of <hslash>
("<varlightning>"	"#21AF") ; variant of <lightning>
("<varnothing>"		"#2205") ; variant of <emptyset>
("<smallsetminus>"	"#2216") ; variant of <setminus>
("<shortmid>"		"#2223") ; variant of <mid>
("<shortparallel>"	"#2225") ; variant of <parallel>
("<nshortparallel>"	"#2226") ; variant of <nparallel>
("<wasytherefore>"	"#2234") ; variant of <therefore>
("<thicksim>"		"#223C") ; variant of <sim>
("<thicksim>"		"#2248") ; variant of <approx>
("<lvertneqq>"		"#2268") ; variant of <lneqq>
("<gvertneqq>"		"#2269") ; variant of <gneqq>
("<varsubsetneq>"	"#228A") ; variant of <subsetneq>
("<varsupsetneq>"	"#228B") ; variant of <supsetneq>
("<oplus>"		"#2295") ; variant of <varoplus>
("<ominus>"		"#2296") ; variant of <varominus>
("<otimes>"		"#2297") ; variant of <varotimes>
("<oslash>"		"#2298") ; variant of <varoslash>
("<odot>"		"#2299") ; variant of <varodot>
("<circledcirc>"	"#22A9") ; variant of <varocircle>
("<circledast>"		"#22AA") ; variant of <varoast>
("<varsubsetneqq>"	"#2ACB") ; variant of <subsetneqq>
("<varsupsetneqq>"	"#2ACC") ; variant of <supsetneqq>

("<Bbbk>"		"#1D55C") ; variant of <bb-k>
