
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-kbd.scm
;; DESCRIPTION : keyboard shortcuts for folding and switching
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-kbd)
  (:use (generic generic-kbd)
	(dynamic fold-edit)))

(kbd-map
  ("ir-menu" (toggle-full-screen-mode))
  ("ir-up" (dynamic-operate-on-buffer :first))
  ("ir-left" (dynamic-traverse-buffer :previous))
  ("ir-center" (dynamic-traverse-buffer :next))
  ("ir-right" (dynamic-traverse-buffer :next))
  ("ir-down" (dynamic-operate-on-buffer :last))
  ("F9" (dynamic-operate-on-buffer :first))
  ("F10" (dynamic-traverse-buffer :previous))
  ("F11" (dynamic-traverse-buffer :next))
  ("F12" (dynamic-operate-on-buffer :last))
  ("S-F9" (dynamic-first))
  ("S-F10" (dynamic-previous))
  ("S-F11" (dynamic-next))
  ("S-F12" (dynamic-last))
  ("C-F9" (toggle-full-screen-mode)))

(kbd-map
  (:mode in-screens?)
  ("pageup" (dynamic-traverse-buffer :previous))
  ("pagedown" (dynamic-traverse-buffer :next)))
