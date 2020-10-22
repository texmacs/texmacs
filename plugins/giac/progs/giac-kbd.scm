
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : maxima-kbd.scm
;; DESCRIPTION : Keyboard shortcuts for Maxima
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;               (C) 2020  Luka Marohnić
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (giac-kbd)
  (:use (dynamic scripts-kbd)))

(texmacs-modes
  (giac-scripts-math% #t giac-scripts% in-math%))

(kbd-map
  (:mode giac-scripts-math?)
  (": =" (insert '(script-assign))))
