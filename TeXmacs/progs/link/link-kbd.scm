
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-kbd.scm
;; DESCRIPTION : keyboard shortcuts for linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-kbd)
  (:use (generic generic-kbd)
	(link link-edit)
	(link link-navigate)
	(link link-extract)))

(kbd-map
  (:mode with-linking-tool?)
  ("link" "" "Linking command")
  ("link L" (make-locus))
  ("link 1" (set-link-mode "simple"))
  ("link 2" (set-link-mode "bidirectional"))
  ("link x" (set-link-mode "external"))
  ("link <" (link-set-locus 0))
  ("link >" (link-set-locus 1))
  ("link u" (interactive link-set-target-url))
  ("link s" (interactive link-set-target-script))
  ("link l" (interactive make-link))
  ("link /" (interactive remove-link-of-types))
  ("link return" (locus-link-follow))
  ("link c" (build-constellation-page))
  ("link i" (build-locus-page))
  ("link e" (interactive build-environment-page)))
