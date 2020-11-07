
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-kbd.scm
;; DESCRIPTION : keyboard shortcuts for on-the-fly evaluation of scripts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-kbd)
  (:use (math math-kbd)
	(dynamic scripts-edit)))

(kbd-map
  ("script *" (make 'script-eval))
  ("script !" (make-script-input))
  ("script =" (toggle-keep-input))
  ("script $" (toggle-eval-math))
  ("script l" (insert-go-to '(converter-eval "latex" "") '(1 0)))
  ("script L" (insert-go-to '(converter-input "latex" "" "") '(1 0)))
  ("script h" (insert-go-to '(converter-eval "html" "") '(1 0)))
  ("script H" (insert-go-to '(converter-input "html" "" "") '(1 0)))
  ("std \\" (insert-go-to '(converter-eval "latex" "") '(1 0))))
