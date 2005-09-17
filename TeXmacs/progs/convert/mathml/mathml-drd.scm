
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathml-drd.scm
;; DESCRIPTION : DRD properties for MathML
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml mathml-drd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordinary symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tm->mathml-constant%
  ("<mathe>" "e")
  ("<mathi>" "&ImaginaryI;")
  ("<mathpi>" "&pi;"))

(drd-table tm->mathml-operator%
  ("&" "&amp;")
  ("<less>" "&lt;")
  ("*" "&InvisibleTimes;")
  (" " "&ApplyFunction;"))

(drd-table mathml-constant->tm%
  ("&ii;" "<mathi>")
  ("&true;" "true")
  ("&false;" "false")
  ("&Copf;" "<bbb-C>")
  ("&Qopf;" "<bbb-Q>")
  ("&Zopf;" "<bbb-Z>")
  ("&Ropf;" "<bbb-R>"))

(drd-table mathml-operator->tm%
  ("&it;" "*")
  ("*" "*")
  ("&RightArrow;" "<rightarrow>"))

(drd-rules
  ((mathml-constant->tm% 'x 'y) (tm->mathml-constant% 'y 'x))
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

(drd-table tm->mathml-right%
  (")" ")")
  ("]" "]")
  ("}" "}")
  ("rangle" "&RightAngleBracket;")
  ("rfloor" "&RightFloor;")
  ("rceil" "&RightCeiling;")
  ("rrbracket" "&RightDoubleBracket;")
  ("\\\\" "&Backslash;"))

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
;;  ("sqcap" "&SquareIntersection;")
;;  ("sqcup" "&SquareUnion;")
;;  ("curlywedge" "&CurlyWedge;")
;;  ("curlyvee" "&CurlyVee;")
;;  ("triangleup" "&TriangleUp;")
;;  ("triangledown" "&TriangleDown;")
;;  ("box" "&Box;")
  ("pluscup" "&UnionPlus;")
;;  ("parallel" "&Parallel;")
;;  ("interleave" "&Interleave;")
)

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
  ((mathml-large->tm% 'x 'y) (tm->mathml-large% 'y 'x))
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
