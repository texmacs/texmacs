;; Special symbol conversion for some unicode characters in math mode

;; (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details. If
;; you don't have this file, write to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
("<AE>"		"#C6")
("<Thorn>"	"#DE")
("<O/>"		"#D8")
("<sz>"		"#DF")
("<ae>"		"#E6")
("<dh>"		"#F0")
("<o/>"		"#F8")
("<thorn>"	"#FE")


;;; Latin Extended-A				0100--017F

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
