
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-menu.scm
;; DESCRIPTION : routines for on-the-fly evaluation of scripts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-menu)
  (:use (dynamic scripts-edit)))

(menu-bind scripts-eval-menu
  (when (script-evaluable?)
    ("Evaluate" (script-eval))
    (if (plugin-approx-command-ref (get-env "prog-scripts"))
	("Approximate" (script-approx))))
  ("Evaluation tag" (make 'script-eval))
  ("Evaluation switch" (make-script-input)))

(menu-bind scripts-eval-toggle-menu
  ("Keep evaluated expressions" (toggle-keep-input))
  ("Quick evaluation of formulas" (toggle-eval-math)))

(menu-bind scripts-plot-menu
  ("Curve" (make 'plot-curve))
  ("Surface" (make 'plot-surface))
  ("Parametric curve" (make 'plot-curve*))
  ("Parametric surface" (make 'plot-surface*)))
