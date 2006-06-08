
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-kbd.scm
;; DESCRIPTION : keyboard shortcuts for linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-kbd)
  (:use (link link-edit) (link link-navigate) (link link-extract)))

(kbd-map
  (:mode with-linking-tool?)
  ("link" "" "Linking command")
  ("link L" (make-locus))
  ("link 1" (set-link-mode "simple"))
  ("link 2" (set-link-mode "bidirectional"))
  ("link x" (set-link-mode "external"))
  ("link <" (link-set-locus 0))
  ("link >" (link-set-locus 1))
  ("link l" (interactive make-link))
  ("link /" (interactive remove-link))
  ("link return" (locus-link-follow))
  ("link c" (build-constellation-page))
  ("link i" (build-locus-page))
  ("link e" (interactive build-environment-page)))
