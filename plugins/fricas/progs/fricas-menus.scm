
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fricas-menus.scm
;; DESCRIPTION : FriCAS menus
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fricas-menus)
  (:use (texmacs texmacs tm-files)
	;(doc help-funcs)
	(dynamic scripts-edit)
        (dynamic session-menu)))

(define fricas-apply script-apply)

(menu-bind fricas-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Calculus"
      ("Differentiate" (fricas-apply "D" 2))
      ("Integrate"     (fricas-apply "integrate" 2))
      ("Limit"         (fricas-apply "limit" 3))
      ---
      ("Sum"     (fricas-apply "sum" 2))
      ("Product" (fricas-apply "product" 2))
      ---
      ("Solve an equation" (fricas-apply "solve" 2)))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))
