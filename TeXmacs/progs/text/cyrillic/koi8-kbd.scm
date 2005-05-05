
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : koi8-kbd.scm
;; DESCRIPTION : typing russian using the koi8 keyboard encoding
;; COPYRIGHT   : (C) 1999-2001  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic koi8-kbd))

(kbd-map in-cyrillic-koi8?
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
