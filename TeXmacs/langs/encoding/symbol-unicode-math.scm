;; Special symbol conversion for some unicode characters in math mode

;; (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

;; Those characters are associated to a Cork character (in text mode) and to a
;; symbol (in math mode). Those Cork characters and universal symbols are
;; exported to the same unicode character, but this unicode character must be
;; imported differently if it is placed in math or text mode.

;;; Control and Basic Latin			0000--007F

("<backslash>"	"#5C")
("<grave>"	"#60")


;;; Controls and Latin-1 Supplement		0080--00FF

("<flip-!>"	"#A1")
("<paragraph>"	"#A7")
("<ddot>"	"#A8")
("<bar>"	"#AF")
("<acute>"	"#B4")
("<cedille>"	"#B8")
("<flip-?>"	"#BF")
("<AA>"		"#C5")
("<AE>"		"#C6")
("<Thorn>"	"#DE")
("<O/>"		"#D8")
("<sz>"		"#DF")
("<aa>"		"#E5")
("<ae>"		"#E6")
("<dh>"		"#F0")
("<o/>"		"#F8")
("<thorn>"	"#FE")


;;; Latin Extended-A				0100--017F

("<imath>"	"#0131")
("<OE>"		"#0152")
("<oe>"		"#0153")


;;; Spacing modifier letters			02B0--02FF

("<check>"	"#02C7")
("<breve>"	"#02D8")
("<dot>"	"#02D9")
("<abovering>"	"#02DA")


;;; General Punctuation				2000--206F

("<-->"		"#2013")
("<--->"	"#2014")
("<``>"		"#201C")
("<permil>"	"#2030")
