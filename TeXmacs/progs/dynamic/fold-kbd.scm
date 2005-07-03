
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-kbd.scm
;; DESCRIPTION : keyboard shortcuts for folding and switching
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-kbd)
  (:use (dynamic fold-edit)))

(kbd-map
  ("F9" (dynamic-operate-on-buffer :first))
  ("F10" (dynamic-traverse-buffer #f))
  ("F11" (dynamic-traverse-buffer #t))
  ("F12" (dynamic-operate-on-buffer :last))
  ("S-F9" (dynamic-first))
  ("S-F10" (dynamic-previous))
  ("S-F11" (dynamic-next))
  ("S-F12" (dynamic-last)))
