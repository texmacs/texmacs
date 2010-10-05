
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for documentation mode
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-kbd)
  (:use (text std-text-kbd)
	(doc tmdoc-drd)))

(kbd-map
  (:mode in-tmdoc?)
  ("S-F5" (make 'scm-arg))
  ("S-F7" (make 'scm))
  ("M-F7" (make 'scm-fragment)))
