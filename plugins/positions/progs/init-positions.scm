
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 2599fdd3-6ac2-4558-b731-ff6fa1178723
;;
;; MODULE      : init-positions.scm
;; DESCRIPTION : Initialize the 'positions' plugin
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

(define (positions-initialize)
  (lazy-define (positions) position-remember-ia)
  (lazy-define (positions) position-jump-ia)
  (lazy-define (positions) position-forget-ia)
  (menu-extend tools-menu
    ---
    (-> "Positions"
	("Remember position" (position-remember-ia))
	("Jump to position" (position-jump-ia))
	("Forget position" (position-forget-ia))))
  (lazy-keyboard (kbd-positions) always?))

(plugin-configure positions
  (:require #t)
  (:initialize (positions-initialize)))
