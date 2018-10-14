
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fricas-kbd.scm
;; DESCRIPTION : Keyboard shortcuts for FriCAS
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fricas-kbd)
  (:use (dynamic scripts-kbd)))

(texmacs-modes
  (fricas-scripts-math% #t fricas-scripts% in-math%))

(kbd-map
  (:mode fricas-scripts-math?)
  (": =" (insert '(script-assign))))
