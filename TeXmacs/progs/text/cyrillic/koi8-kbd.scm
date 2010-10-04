
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : koi8-kbd.scm
;; DESCRIPTION : typing russian using the koi8 keyboard encoding
;; COPYRIGHT   : (C) 1999-2001  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic koi8-kbd)
  (:use (text text-kbd)))

(kbd-map
  (:mode in-cyrillic-koi8?)

  ("Á" "à")
  ("Â" "á")
  ("×" "â")
  ("Ç" "ã")
  ("Ä" "ä")
  ("Å" "å")
  ("Ö" "æ")
  ("Ú" "ç")
  ("É" "è")
  ("Ê" "é")
  ("Ë" "ê")
  ("Ì" "ë")
  ("Í" "ì")
  ("Î" "í")
  ("Ï" "î")
  ("Ð" "ï")
  ("Ò" "ð")
  ("Ó" "ñ")
  ("Ô" "ò")
  ("Õ" "ó")
  ("Æ" "ô")
  ("È" "õ")
  ("Ã" "ö")
  ("Þ" "÷")
  ("Û" "ø")
  ("Ý" "ù")
  ("ß" "ú")
  ("Ù" "û")
  ("Ø" "ü")
  ("Ü" "ý")
  ("À" "þ")
  ("Ñ" "ÿ")
  ("£" "¼")
  ("accent:umlaut Å" "¼")

  ("á" "À")
  ("â" "Á")
  ("÷" "Â")
  ("ç" "Ã")
  ("ä" "Ä")
  ("å" "Å")
  ("ö" "Æ")
  ("ú" "Ç")
  ("é" "È")
  ("ê" "É")
  ("ë" "Ê")
  ("ì" "Ë")
  ("í" "Ì")
  ("î" "Í")
  ("ï" "Î")
  ("ð" "Ï")
  ("ò" "Ð")
  ("ó" "Ñ")
  ("ô" "Ò")
  ("õ" "Ó")
  ("æ" "Ô")
  ("è" "Õ")
  ("ã" "Ö")
  ("þ" "×")
  ("û" "Ø")
  ("ý" "Ù")
  ("ÿ" "Ú")
  ("ù" "Û")
  ("ø" "Ü")
  ("ü" "Ý")
  ("à" "Þ")
  ("ñ" "ß")
  ("³" "œ")
  ("accent:umlaut å" "œ"))
