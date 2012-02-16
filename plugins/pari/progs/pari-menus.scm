
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : pari-menus.scm
;; DESCRIPTION : Menus for the pari plugin
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
                      (concat :%1 (math (with "color" "blue" :%1)))))
	 `(math ,(plugin-output-simplify name (tm-ref t 2 1 0 2))))
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

  (menu-bind plugin-menu
    (:require (or (in-pari?) (and (not-in-session?) (pari-scripts?))))
    (=> "Pari"
        (link pari-menu)))
