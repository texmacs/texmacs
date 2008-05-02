
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for documentation mode
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-kbd)
  (:use (doc tmdoc-drd)))

(kbd-map
  (:mode in-tmdoc?)
  ("S-F5" (make 'scm-arg))
  ("S-F7" (make 'scm))
  ("M-F7" (make 'scm-fragment)))
