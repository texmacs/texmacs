
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-kbd.scm
;; DESCRIPTION : Keyboard shortcuts for automated document generation
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils automate auto-kbd)
  (:use (utils automate auto-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-auto?)
  ("I F" (make-block-if))
  ("I F var" (make-block-if-else))
  ("F O R" (make-block-for))
  ("W H I L E" (make-block-while))
  ("T A G" (make-block-tag))
  ("< - -" (make-block-assign))
  ("O U T" (make-block-output))
  ("I f" (make-inline-if))
  ("I f var" (make-inline-if-else))
  ("F o r" (make-inline-for))
  ("W h i l e" (make-inline-while))
  ("T a g" (make-inline-tag))
  ("< -" (make-inline-assign))
  ("O u t" (make-inline-output))
  ("$ $" (make-output-string)))
