
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : yawerty-kbd.scm
;; DESCRIPTION : typing russian using the yawerty keyboard encoding
;; COPYRIGHT   : (C) 1999-2001  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic yawerty-kbd)
  (:use (text text-kbd)))

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
