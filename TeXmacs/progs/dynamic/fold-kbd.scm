
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

(define (reset-buffer-players)
  (players-set-elapsed (buffer-tree) 0.0)
  (update-players (tree->path (buffer-tree)) #f))

(kbd-map
  ("ir-up" (screens-switch-to :previous))
  ("ir-down" (screens-switch-to :next))
  ("ir-left" (dynamic-traverse-buffer :previous))
  ("ir-right" (dynamic-traverse-buffer :next))
  ("ir-center" (dynamic-traverse-buffer :next))
  ("ir-play" (dynamic-traverse-buffer :next))
  ("ir-pause" (dynamic-traverse-buffer :next))
  ("ir-menu" (toggle-full-screen-mode))
  ("F9" (dynamic-operate-on-buffer :first))
  ("F10" (dynamic-traverse-buffer :previous))
  ("F11" (dynamic-traverse-buffer :next))
  ("F12" (dynamic-operate-on-buffer :last))
  ("S-F9" (dynamic-first))
  ("S-F10" (dynamic-previous))
  ("S-F11" (dynamic-next))
  ("S-F12" (dynamic-last))
  ("C-F9" (toggle-full-screen-mode))
  ("C-F10" (toggle-panorama-mode))
  ("C-F11" (toggle-remote-control-mode))
  ("C-F12" (reset-buffer-players)))

(kbd-map
  (:mode in-screens?)
  ("pageup" (screens-switch-to :previous))
  ("pagedown" (screens-switch-to :next)))
