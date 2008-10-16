
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 5b8158a8-7ade-43d9-bebb-3277e6c07618
;;
;; MODULE      : kbd-positions.scm
;; DESCRIPTION : Keybindings for the "positions" plugin.
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

(texmacs-module (kbd-positions))

(lazy-define (positions) position-remember-ia)
(lazy-define (positions) position-jump-ia)
(lazy-define (positions) position-forget-ia)

(kbd-map
  ("emacs:prefix M-p" (position-remember-ia))
  ("emacs:prefix p" (position-jump-ia))
  ("emacs:prefix A-p" (position-forget-ia)))
