;; Fall-back conversion of some TeXmacs symbols to Unicode.

;; (C) 2003  David Allouche
;; (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

;; Those symbols have a sensible unicode translation, so they can be exported.
;; But another symbol was chosen for the importation of the corresponding
;; unicode character. So the exportation is one-way only.

;;; Long arrows

;; These arrows have no long variant in unicode. They are exported as regular
;; arrows.

;; No long "two headed" arrow
("<longtwoheadleftarrow>"	"#219E")
("<longtwoheadrightarrow>"	"#21A0")

;; No "long upwards arrow" or "long downwards arrow"
("<longuparrow>"		"#2191")
("<longdownarrow>"		"#2193")
("<longupdownarrow>"		"#2195")
("<Longuparrow>"		"#21D1")
("<Longdownarrow>"		"#21D3")
("<Longupdownarrow>"		"#21D5")
;("<longmapsup>"		"")
;("<longmapsdown>"		"")
