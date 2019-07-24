
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cp1251-kbd.scm
;; DESCRIPTION : typing russian using the cp1251 keyboard encoding
;; COPYRIGHT   : (C) 1999-2001  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text cyrillic cp1251-kbd)
  (:use (text text-kbd)))

(kbd-map
  (:mode in-cyrillic-cp1251?)
  ("¸" "<#451>")
  ("accent:umlaut <#435>" "<#451>")
  ("¨" "<#401>")
  ("accent:umlaut <#415>" "<#401>"))
