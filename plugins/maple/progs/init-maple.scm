
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-maple.scm
;; DESCRIPTION : Initialize maple plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure maple
  (:macpath "/Library/Frameworks/Maple.framework/Versions/1*" "bin")
  (:require (url-exists-in-path? "maple"))
  (:launch "tm_maple")
  (:session "Maple"))

(tm-cond-expand (supports-maple?)
  (lazy-input-converter (maple-input) maple))
