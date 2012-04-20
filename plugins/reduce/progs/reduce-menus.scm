
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : reduce-menus.scm
;; DESCRIPTION : Reduce menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (reduce-menus)
  (:use (texmacs texmacs tm-files)
	(doc help-funcs)
	(dynamic scripts-edit)))

(define reduce-apply script-apply)
(define (reduce-package package) (insert (string-append "load_package " package ";")))

(menu-bind reduce-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Packages"
      (-> "Calculis"
          ("Integrals with sqrt" (reduce-package "algint"))
          ("Definite integrals"  (reduce-package "defint"))
          ---
          ("Truncated power series" (reduce-package "tps"))
          ("Taylor series"          (reduce-package "taylor"))
          ("Formal power series"    (reduce-package "fps"))
          ("Series summation"       (reduce-package "sum"))
          ("Summation"              (reduce-package "zeilberg"))
          ("Summing q-hypergeometric terms" (reduce-package "qsum"))
          ---
          ("Limits"                      (reduce-package "limits"))
          ("Limits of exp-log functions" (reduce-package "mrvlimits"))
          ---
          ("Derivatives of generic functions" (reduce-package "dfpart"))
          ---
          ("Residues" (reduce-package "residue"))
          ---
          ("Laplace transforsms" (reduce-package "laplace"))
          ("Z transforsms"       (reduce-package "ztrans"))
          ---
          ("Differential geometry"         (reduce-package "excalc"))
          ("Exterior differential systems" (reduce-package "eds")))
      (-> "Functions"
          ("Special functions"                     (reduce-package "specfn"))
          ("Hypergeometric and Meijer G functions" (reduce-package "specfn2")))
      (-> "Simplification"
          ("Trigonometric functions"               (reduce-package "trigsimp"))
          ("Expressions with dependent varianbles" (reduce-package "compact")))
      (-> "Solving equations"
          ("Roots of polynomials"  (reduce-package "roots"))
          ("Recurrence relations"  (reduce-package "rsolve"))
          ("Modular polynomials"   (reduce-package "modsr"))
          ("Inequalities"          (reduce-package "ineq"))
          ---
          ("Ordinary differential equations" (reduce-package "odesolve"))
          ("Change of variables in DEs"      (reduce-package "changevar"))
          ("Linear DE near a singular point" (reduce-package "desir"))
          ("Symmetries of DEs"               (reduce-package "applysym"))
          ("Overdetermined systems of DEs"   (reduce-package "crack"))
          ---
          ("Symmetries of PDEs"       (reduce-package "spde"))
          ("Geometry of PDEs"         (reduce-package "cdiff"))
          ("Finite-difference method" (reduce-package "fide")))

      (-> "Linear algebra, vectors, tensors"
          ("Linear algebra"     (reduce-package "linalg"))
          ("Normal forms"       (reduce-package "normform"))
          ("Sparse matrices"    (reduce-package "sparse"))
          ("Symmetric matrices" (reduce-package "symmetry"))
          ---
          ("3d vectors"                           (reduce-package "avector"))
          ("3d vectors in orthogonal coordinates" (reduce-package "orthovec"))
          ("Tensors with symmetries"              (reduce-package "atensor"))
          ("Dummy indices"                        (reduce-package "dummy")))
      (-> "Groebner bases"
          ("Groebner bases"              (reduce-package "groebner"))
          ("With parameters"             (reduce-package "cgb"))
          ("Polynomial ideals"           (reduce-package "ideals"))
          ("Exterior algebra"            (reduce-package "xideal"))
          ("Non-commutative polynomials" (reduce-package "ncpoly"))
          ("Involutive bases"            (reduce-package "invbase"))
          ("Wu algorithm"                (reduce-package "wu"))
          ("Commutative algebra"         (reduce-package "cali"))
          ("Random polynomials"          (reduce-package "randpoly")))
      (-> "Aggebra"
          ("Algebraic numbers" (reduce-package "arnum"))
          ("Lie symmetries"    (reduce-package "lie")))
      (-> "Logic"
          ("Boolean algebra"      (reduce-package "boolean"))
          ("First-order formulas" (reduce-package "redlog"))
          ("Finite sets"          (reduce-package "sets")))
      (-> "Physics, chemistry"
          ("Dirac matrices"              (reduce-package "cvit"))
          ("Color factors"               (reduce-package "xcolor"))
          ("Supersymmetry"               (reduce-package "susy2"))
          ("Quantum mechanics operators" (reduce-package "physop"))
          ("Celestial mechanics"         (reduce-package "camal"))
          ---
          ("Chemical reaction equations" (reduce-package "reacteqn")))
      (-> "Code generation"
          ("Fortran, C, Pascal" (reduce-package "gentran"))
          ("Optimization"       (reduce-package "scope"))
          ---
          ("LaTeX"  (reduce-package "rlfi"))
          ("TeX"    (reduce-package "tri"))
          ("mathml" (reduce-package "mathml")))
      ("Numeric calculations"    (reduce-package "numeric"))
      ("Plots"                   (reduce-package "gnuplot"))
      ("Pattern matching"        (reduce-package "pm"))
      ("Rational approximations" (reduce-package "rataprx"))
      ("General utilities"       (reduce-package "assist"))
      ("Debugging"               (reduce-package "rdebug"))
      ("Reset"                   (reduce-package "reset")))
  (-> "Calculus"
      ("Differentiate" (reduce-apply "df" 2))
      ("Integrate"     (reduce-apply "int" 2))
      ("Limit"         (reduce-apply "limit" 3))
      ---
      ("Sum"     (reduce-apply "sum" 3))
      ("Product" (reduce-apply "prod" 3))
      ---
      ("Solve an equation" (reduce-apply "solve" 2)))
  (-> "Polynomials and rational functions"
      ("Factorize"            (reduce-apply "factorize"))
      ("List of coefficients" (reduce-apply "coeff" 2))
      ("n-th coefficient"     (reduce-apply "coeffn" 3))
      ---
      ("Numerator"         (reduce-apply "num"))
      ("Denominator"       (reduce-apply "den"))
      ("Partial fractions" (reduce-apply "pf" 2)))
  (-> "Linear algebra"
      ("Determinant" (reduce-apply "det"))
      ("Trace"       (reduce-apply "trace"))
      ("Transpose"   (reduce-apply "tp")))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind reduce-help-icons
  (if (and (in-reduce?) reduce-help)
      /
      ((balloon (icon "tm_help.xpm") "Reduce manual")
       (load-buffer-in-new-window reduce-help))))

(menu-bind session-help-icons
  (:require (and (in-reduce?) (in-session?)))
  (link reduce-help-icons))

(menu-bind plugin-menu
  (:require (or (in-reduce?) (and (not-in-session?) (reduce-scripts?))))
  (=> "Reduce" (link reduce-menu)))
