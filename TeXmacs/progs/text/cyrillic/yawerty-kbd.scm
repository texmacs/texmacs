
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : yawerty-kbd.scm
;; DESCRIPTION : typing russian using the yawerty keyboard encoding
;; COPYRIGHT   : (C) 1999-2001  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic yawerty-kbd))

(kbd-map
  (:mode in-cyrillic-yawerty?)

  ("q" "ÿ")
  ("w" "â")
  ("e" "å")
  ("r" "ð")
  ("t" "ò")
  ("y" "û")
  ("u" "ó")
  ("i" "è")
  ("o" "î")
  ("p" "ï")
  ("[" "ø")
  ("]" "ù")
  ("a" "à")
  ("s" "ñ")
  ("d" "ä")
  ("f" "ô")
  ("g" "ã")
  ("h" "õ")
  ("j" "é")
  ("k" "ê")
  ("l" "ë")
  ("z" "ç")
  ("x" "ü")
  ("c" "ö")
  ("v" "æ")
  ("b" "á")
  ("n" "í")
  ("m" "ì")
  ("\\" "ý")
  ("`" "þ")
  ("=" "÷")

  ("Q" "ß")
  ("W" "Â")
  ("E" "Å")
  ("R" "Ð")
  ("T" "Ò")
  ("Y" "Û")
  ("U" "Ó")
  ("I" "È")
  ("O" "Î")
  ("P" "Ï")
  ("{" "Ø")
  ("}" "Ù")
  ("A" "À")
  ("S" "Ñ")
  ("D" "Ä")
  ("F" "Ô")
  ("G" "Ã")
  ("H" "Õ")
  ("J" "É")
  ("K" "Ê")
  ("L" "Ë")
  ("Z" "Ç")
  ("X" "Ü")
  ("C" "Ö")
  ("V" "Æ")
  ("B" "Á")
  ("N" "Í")
  ("M" "Ì")
  ("|" "Ý")
  ("~" "Þ")
  ("+" "×")

  ("#" "ú")
  ("$" "Ú")
  ("^" "¼")
  ("&" "œ")

  ("accent:umlaut e" "¼")
  ("accent:umlaut E" "œ"))
