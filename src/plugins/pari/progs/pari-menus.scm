
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : pari-menus.scm
;; DESCRIPTION : Menus for the pari plugin
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (pari-menus)
  (:use (utils plugins plugin-cmd)
	(doc help-funcs)
	(dynamic scripts-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Pari expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (plugin-output-simplify name t)
  (:require (== name "pari"))
  (cond ((match? t '(with "color" "magenta"
		      (concat :%1 (with "mode" "math" "color" "blue" :%1))))
	 `(math ,(plugin-output-simplify name (tm-ref t 2 1 4))))
	(else (plugin-output-std-simplify name t))))

(define pari-apply script-apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Pari menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind pari-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Elementary functions"
      ("exp" (pari-apply "exp"))
      ("log" (pari-apply "log"))
      ("sqrt" (pari-apply "sqrt"))
      ---
      ("cos" (pari-apply "cos"))
      ("sin" (pari-apply "sin"))
      ("tan" (pari-apply "tan"))
      ("arccos" (pari-apply "acos"))
      ("arcsin" (pari-apply "asin"))
      ("arctan" (pari-apply "atan"))
      ---
      ("ch" (pari-apply "cosh"))
      ("sh" (pari-apply "sinh"))
      ("th" (pari-apply "tanh"))
      ("argch" (pari-apply "acosh"))
      ("argsh" (pari-apply "asinh"))
      ("argth" (pari-apply "atanh")))
  (-> "Transcendental functions"
      ("Erfc" (pari-apply "erfc"))
      ("Gamma" (pari-apply "gamma"))
      ("Psi" (pari-apply "psi"))
      ("Theta" (pari-apply "theta" 2))
      ("Zeta" (pari-apply "zeta")))
  (-> "Number theoretical functions"
      ("Factor" (pari-apply "factor"))
      ("Gcd" (pari-apply "gcd"))
      ("Lcm" (pari-apply "lcm"))
      ("Next prime" (pari-apply "nextprime"))
      ("N-th prime" (pari-apply "prime")))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))
