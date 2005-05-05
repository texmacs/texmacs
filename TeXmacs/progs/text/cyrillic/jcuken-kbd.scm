
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : jcuken-kbd.scm
;; DESCRIPTION : typing russian using the jcuken keyboard encoding
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic jcuken-kbd))

(kbd-map in-cyrillic-jcuken?
  ("q" "é")
  ("w" "ö")
  ("e" "ó")
  ("r" "ê")
  ("t" "å")
  ("y" "í")
  ("u" "ã")
  ("i" "ø")
  ("o" "ù")
  ("p" "ç")
  ("[" "õ")
  ("]" "ú")
  ("a" "ô")
  ("s" "û")
  ("d" "â")
  ("f" "à")
  ("g" "ï")
  ("h" "ð")
  ("j" "î")
  ("k" "ë")
  ("l" "ä")
  (";" "æ")
  ("'" "ý")
  ("z" "ÿ")
  ("x" "÷")
  ("c" "ñ")
  ("v" "ì")
  ("b" "è")
  ("n" "ò")
  ("m" "ü")
  ("," "á")
  ("." "þ")
  ("`" "¼")

  ("Q" "É")
  ("W" "Ö")
  ("E" "Ó")
  ("R" "Ê")
  ("T" "Å")
  ("Y" "Í")
  ("U" "Ã")
  ("I" "Ø")
  ("O" "Ù")
  ("P" "Ç")
  ("{" "Õ")
  ("}" "Ú")
  ("A" "Ô")
  ("S" "Û")
  ("D" "Â")
  ("F" "À")
  ("G" "Ï")
  ("H" "Ð")
  ("J" "Î")
  ("K" "Ë")
  ("L" "Ä")
  (":" "Æ")
  ("\"" "Ý")
  ("Z" "ß")
  ("X" "×")
  ("C" "Ñ")
  ("V" "Ì")
  ("B" "È")
  ("N" "Ò")
  ("M" "Ü")
  ("<" "Á")
  (">" "Þ")
  ("~" "œ")

  ("@" "\"")
  ("#" "'")
  ("$" "*")
  ("%" ":")
  ("^" ",")
  ("&" ".")
  ("*" ";")

  ("accent:umlaut t" "¼")
  ("accent:umlaut T" "œ"))
