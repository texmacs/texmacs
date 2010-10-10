
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathml-drd.scm
;; DESCRIPTION : DRD properties for MathML
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml mathml-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordinary symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tm->mathml-constant%
  ("<mathcatalan>" "C")
  ("<mathe>" "e")
  ("<matheuler>" "&eulergamma;")
  ("<mathi>" "&ImaginaryI;")
  ("<mathpi>" "&pi;"))

(drd-table mathml-constant->tm%
  ("&eulergamma;" "<matheuler>")
  ("&ImaginaryI;" "<mathi>")
  ("&ii;" "<mathi>")
  ("&pi;" "<mathpi>")
  ("&true;" "true")
  ("&false;" "false")
  ("&phiv;" "<varphi>")
  ("&ell;" "<ell>")
  ("&Copf;" "<bbb-C>")
  ("&Qopf;" "<bbb-Q>")
  ("&Zopf;" "<bbb-Z>")
  ("&Ropf;" "<bbb-R>"))

(drd-table tm->mathml-operator%
  ("&" "&amp;")
  ("<less>" "&lt;")
  ("*" "&InvisibleTimes;")
  (" " "&ApplyFunction;"))

(drd-table mathml-operator->tm%
  ("&it;" "*")
  ("*" "*")
  ("&leq;" "<leq>")
  ("&geq;" "<geq>")
  ("&RightArrow;" "<rightarrow>"))

(drd-table mathml-symbol->tm%
  ("&mldr;" "<cdots>")
  ("&ThinSpace;" (hspace "0.1666667em"))
  ("&MediumSpace;" (hspace "0.2222222em"))
  ("&ThickSpace;" (hspace "0.2777778em")))

(drd-rules
  ((mathml-operator->tm% 'x 'y) (tm->mathml-operator% 'y 'x))
  ((mathml-symbol->tm% 'x 'y) (mathml-constant->tm% 'x 'y))
  ((mathml-symbol->tm% 'x 'y) (mathml-operator->tm% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tm->mathml-left%
  ("(" "(")
  ("[" "[")
  ("{" "{")
  ("langle" "&LeftAngleBracket;")
  ("lfloor" "&LeftFloor;")
  ("lceil" "&LeftCeiling;")
  ("llbracket" "&LeftDoubleBracket;")
  ("/" "/"))

(drd-table tmtm-left%
  ;; For HTML entities &lfloor;, &lceil, etc.
  ("<lfloor>" "lfloor")
  ("<lceil>" "lceil")
  ("<langle>" "langle"))

(drd-table tm->mathml-right%
  (")" ")")
  ("]" "]")
  ("}" "}")
  ("rangle" "&RightAngleBracket;")
  ("rfloor" "&RightFloor;")
  ("rceil" "&RightCeiling;")
  ("rrbracket" "&RightDoubleBracket;")
  ("\\\\" "&Backslash;"))

(drd-table tmtm-right%
  ;; For HTML entities &rfloor;, &rceil, etc.
  ("<rfloor>" "rfloor")
  ("<rceil>" "rceil")
  ("<rangle>" "rangle"))

(drd-table tm->mathml-big%
  ("sum" "&Sum;")
  ("prod" "&Product;")
  ("int" "&Integral;")
  ("oint" "&ContourIntegral;")
  ("amalg" "&Coproduct;")
  ("cap" "&Intersection;")
  ("cup" "&Union;")
  ("wedge" "&Wedge;")
  ("vee" "&Vee;")
  ("odot" "&CircleDot;")
  ("oplus" "&CirclePlus;")
  ("otimes" "&CircleTimes;")
  ("sqcap" "&SquareIntersection;")     ;; FIXME: displayed too small
  ("sqcup" "&SquareUnion;")            ;; FIXME: displayed too small
  ;;("curlywedge" "&CurlyWedge;")
  ;;("curlyvee" "&CurlyVee;")
  ;;("triangleup" "&TriangleUp;")
  ;;("triangledown" "&TriangleDown;")
  ;;("box" "&Box;")
  ("pluscup" "&UnionPlus;")
  ;;("parallel" "&Parallel;")
  ;;("interleave" "&Interleave;")
)

(drd-table tmtm-big%
  ;; For HTML entities &sum;, &prod;, etc.
  ("<sum>" "sum")
  ("<prod>" "prod")
  ("<int>" "int"))

(drd-table tm->mathml-above-below%
  ("^" "&Hat;")
  ("~" "&Tilde;")
  ("<bar>" "&OverBar;")
  ("<vect>" "&RightVector;")
  ("<check>" "&Hacek;")
  ("<breve>" "&Breve;")
  ("<acute>" "&DiacriticalAcute;")
  ("<grave>" "&DiacriticalGrave;")
  ("<dot>" "&DiacriticalDot;")
  ("<ddot>" "&DoubleDot;")
;;  ("<abovering>" "&AboveRing;")
  ("<wide-varrightarrow>" "&RightArrow;")
  ("<wide-varleftarrow>" "&LeftArrow;"))

(drd-table tm->mathml-only-above%
  ("<wide-overbrace>" "&OverBrace;")
  ("<wide-underbrace*>" "&UnderBrace;")
  ("<wide-sqoverbrace>" "&OverBracket;")
  ("<wide-squnderbrace*>" "&UnderBracket;"))

(drd-table tm->mathml-only-below%
  ("<wide-overbrace*>" "&OverBrace;")
  ("<wide-underbrace>" "&UnderBrace;")
  ("<wide-sqoverbrace*>" "&OverBracket;")
  ("<wide-squnderbrace>" "&UnderBracket;"))

(drd-rules
  ((tm->mathml-large% 'x 'y) (tm->mathml-left% 'x 'y))
  ((tm->mathml-large% 'x 'y) (tm->mathml-right% 'x 'y))
  ((mathml-left->tm% 'x 'y) (tm->mathml-left% 'y 'x))
  ((mathml-right->tm% 'x 'y) (tm->mathml-right% 'y 'x))
  ((mathml-large->tm% 'x 'y) (mathml-left->tm% 'x 'y))
  ((mathml-large->tm% 'x 'y) (mathml-right->tm% 'x 'y))
  ((mathml-big->tm% 'x 'y) (tm->mathml-big% 'y 'x))
  ((tm->mathml-above% 'x 'y) (tm->mathml-only-above% 'x 'y))
  ((tm->mathml-above% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-below% 'x 'y) (tm->mathml-only-below% 'x 'y))
  ((tm->mathml-below% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-only-below% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-only-above% 'x 'y))
  ((tm->mathml-wide*% 'x 'y) (tm->mathml-above-below% 'x 'y))
  ((tm->mathml-wide% 'x 'y) (tm->mathml-wide*% 'x 'y))
  ((tm->mathml-wide% "<wide-bar>" "&OverBar;"))
  ((mathml-above->tm% 'x 'y) (tm->mathml-above% 'y 'x))
  ((mathml-below->tm% 'x 'y) (tm->mathml-below% 'y 'x))
  ((mathml-wide->tm% 'x 'y) (tm->mathml-wide*% 'y 'x)))
