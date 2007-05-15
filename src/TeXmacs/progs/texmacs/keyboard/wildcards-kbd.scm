
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : wildcards-kbd.scm
;; DESCRIPTION : setup keyboard wildcards
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard wildcards-kbd)
  (:use (texmacs keyboard config-kbd)))

(kbd-wildcards
  ("Mod1-" "" #t)
  ("Mod2-" "" #t)
  ("Mod3-" "" #t)
  ("Mod4-" "" #t)
  ("Mod5-" "" #t)

  ("tilde tilde" "tilde")
  ("hat hat" "hat")
  ("umlaut umlaut" "umlaut")
  ("acute acute" "acute")
  ("grave grave" "grave")
  ("cedilla cedilla" "cedilla")
  ("breve breve" "breve")
  ("check check" "check")
  ("doubleacute doubleacute" "doubleacute")
  ("abovering abovering" "abovering")
  ("abovedot abovedot" "abovedot")
  ("ogonek ogonek" "ogonek")

  ,@(compute-wildcard-lines))
