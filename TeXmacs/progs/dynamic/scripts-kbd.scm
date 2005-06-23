
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-kbd.scm
;; DESCRIPTION : keyboard shortcuts for on-the-fly evaluation of scripts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-kbd)
  (:use (dynamic scripts-edit)))

(kbd-map
  ("C-return" (script-eval))
  ("C-!" (insert 'script-eval))
  ("script *" (make-script-input))
  ("script l" (insert-go-to '(converter-eval "latex" "") '(1 0)))
  ("script L" (insert-go-to '(converter-input "latex" "" "") '(1 0)))
  ("script h" (insert-go-to '(converter-eval "html" "") '(1 0)))
  ("script H" (insert-go-to '(converter-input "html" "" "") '(1 0))))
