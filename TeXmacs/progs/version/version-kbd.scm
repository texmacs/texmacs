
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-kbd.scm
;; DESCRIPTION : keyboard shortcuts for versioning
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-kbd)
  (:use (generic generic-kbd)
	(version version-compare)))

(kbd-map
  (:mode with-versioning-tool?)
  ("version home" (version-first-difference))
  ("version pageup" (version-previous-difference))
  ("version pagedown" (version-next-difference))
  ("version end" (version-last-difference))
  ("version up" (version-previous-difference))
  ("version down" (version-next-difference))
  ("version |" (version-show 'version-both))
  ("version left" (version-show 'version-old))
  ("version right" (version-show 'version-new))
  ("version return" (version-select-current))
  ("version 1" (version-select-old))
  ("version 2" (version-select-new)))

(kbd-map
  (:mode with-versioning-tool?)
  (:context version-context?)
  ("C-up" (version-previous-difference))
  ("C-down" (version-next-difference))
  ("C-|" (version-show 'version-both))
  ("C-left" (version-show 'version-old))
  ("C-right" (version-show 'version-new))
  ("C-return" (version-retain 'current))
  ("C-1" (version-retain 0))
  ("C-2" (version-retain 1)))
