
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-handlers.scm
;; DESCRIPTION : Default keyboard handlers
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui kbd-handlers)
  (:use (kernel texmacs tm-define)))

(tm-define (keyboard-press key time)
  (key-press key))

(tm-define (keyboard-focus has-focus? time)
  (noop))

(tm-define (mouse-event key x y mods time)
  ;;(display* "mouse-event " key ", " x ", " y ", " mods ", " time "\n")
  (mouse-any key x y mods (+ time 0.0)))
