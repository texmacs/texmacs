
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-kbd.scm
;; DESCRIPTION : keyboard shortcuts for educational styles
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-kbd)
  (:use (education edu-edit)))

(kbd-map
  (:mode in-edu-text?)
  ("text c" (make-mc 'mc))

  (". . ." (make 'gap))
  (". . . var" "...")
  (". . . ." (make 'gap-wide))
  (". . . . var" "....")
  (". . . . ." (make 'gap-long))
  (". . . . . var" ".....")
  (". . . . . ." "......"))

(kbd-map
  (:mode in-edu-math?)
  (". . ." (make 'gap))
  (". . . ." (make 'gap-wide)))
