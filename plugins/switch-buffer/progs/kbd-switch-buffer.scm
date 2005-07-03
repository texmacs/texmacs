
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 398546c3-c1d0-4348-8c9d-df82493f4847
;;
;; MODULE      : kbd-switch-buffer.scm
;; DESCRIPTION : Keybindings for the 'switch-buffer' plugin.
;; COPYRIGHT   : (C) 2004  David Allouche <david@allouche.net>
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kbd-switch-buffer))

(lazy-define (switch-buffer switch-buffer) interactive-switch-to-buffer)

(kbd-map
  ("emacs:prefix b" (interactive-switch-to-buffer)))
