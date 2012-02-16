
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : giac-menus.scm
;; DESCRIPTION : Giac menus
;; COPYRIGHT   : (C) 1999  Bernard Parisse and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (giac-menus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert Giac primitive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (giac-cursor-pos l)
  (cond ((null? l) 0)
	((null? (cdr l)) 1)
	((and (== (car l) #\() (== (cadr l) #\))) 1)
	((and (== (car l) #\() (== (cadr l) #\,)) 1)
	((and (== (car l) #\,) (== (cadr l) #\))) 1)
	((and (== (car l) #\,) (== (cadr l) #\,)) 1)
	(else (+ (giac-cursor-pos (cdr l)) 1))))

(define (giac-insert s)
  (insert-go-to s (list (giac-cursor-pos (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Giac menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind giac-functions-menu
  ("Normal" (giac-insert "normal()"))
  ("Factor" (giac-insert "factor()"))
  ("Simplify" (giac-insert "simplify()"))
  ("Partfrac" (giac-insert "partfrac()"))
  ---
  (-> "Plot"
      ("Plot setup" (giac-insert "xyztrange()"))
      ("Plotfunc" (giac-insert "plotfunc()"))
      ("Plotparam" (giac-insert "plotparam()"))
      ("Plotpolar" (giac-insert "plotpolar()"))
      ("Plotseq" (giac-insert "plotseq()")))
  ---
  (-> "Constants"
      ("e" (giac-insert "e"))
      ("i" (giac-insert "i"))
      ("pi" (giac-insert "pi"))
      ("+infinity" (giac-insert "+infinity"))
      ("-infinity" (giac-insert "-infinity"))
      ("infinity" (giac-insert "infinity"))
      ("undef" (giac-insert "undef")))
  (-> "Real"
      ("Abs" (giac-insert "abs()"))
      ("Factorial" (giac-insert "factorial()"))
      ("Max" (giac-insert "max()"))
      ("Min" (giac-insert "min()"))
      ("Sign" (giac-insert "sign()")))
  (-> "Complex"
      ("Abs" (giac-insert "abs()"))
      ("Arg" (giac-insert "arg()"))
      ("Conj" (giac-insert "conj()"))
      ("Im" (giac-insert "im()"))
      ("Re" (giac-insert "re()")))
  (-> "Exponential functions"
      ("Exp" (giac-insert "exp()"))
      ("Ln" (giac-insert "ln()"))
      ("Log10" (giac-insert "log10()")))
  (-> "Trigonometric functions"
      ("Acos" (giac-insert "acos()"))
      ("Asin" (giac-insert "asin()"))
      ("Atan" (giac-insert "atan()"))
      ("Cos" (giac-insert "cos()"))
      ("Sin" (giac-insert "sin()"))
      ("Tan" (giac-insert "tan()")))
  (-> "Hyperbolic functions"
      ("Acosh" (giac-insert "acosh()"))
      ("Asinh" (giac-insert "asinh()"))
      ("Atanh" (giac-insert "atanh()"))
      ("Cosh" (giac-insert "cosh()"))
      ("Sinh" (giac-insert "sinh()"))
      ("Tanh" (giac-insert "tanh()")))
  (-> "Special functions"
      ("Airy_Ai" (giac-insert "Airy_Ai()"))
      ("Airy_Bi" (giac-insert "Airy_Bi"))
      ("Gamma" (giac-insert "gamma()"))
      ("Psi" (giac-insert "psi()"))
      ("Zeta" (giac-insert "zeta()")))
  ---
  (-> "Integer arithmetic"
      ("Gcd" (giac-insert "gcd()"))
      ("Lcm" (giac-insert "lcm()"))
      ("Euler" (giac-insert "euler()"))
      ("Iabcuv" (giac-insert "iabcuv()"))
      ("Ichinrem" (giac-insert "ichinrem()"))
      ("Iegcd" (giac-insert "iegcd()"))
      ("Ifactor" (giac-insert "ifactor()"))
      ("Ifactors" (giac-insert "ifactors()"))
      ("Iquo" (giac-insert "iquo()"))
      ("Irem" (giac-insert "irem()"))
      ("Is_prime" (giac-insert "is_prime()"))
      ("Nextprime" (giac-insert "nextprime()"))
      ("Pa2b2" (giac-insert "pa2b2()"))
      ("Prevprime" (giac-insert "prevprime()")))
  (-> "Polynomial arithmetic"
      ("Gcd" (giac-insert "gcd()"))
      ("Lcm" (giac-insert "lcm()"))
      ("Abcuv" (giac-insert "abcuv()"))
      ("Chinrem" (giac-insert "chinrem()"))
      ("Cyclotomic" (giac-insert "cyclotomic()"))
      ("Divis" (giac-insert "divis()"))
      ("E2r" (giac-insert "e2r()"))
      ("Egcd" (giac-insert "egcd()"))
      ("Factor" (giac-insert "factor()"))
      ("Hermite" (giac-insert "hermite()"))
      ("Laguerre" (giac-insert "laguerre()"))
      ("Pcoeff" (giac-insert "pcoeff()"))
      ("Peval" (giac-insert "peval()"))
      ("Proot" (giac-insert "proot()"))
      ("Quo" (giac-insert "quo()"))
      ("Quorem" (giac-insert "quorem()"))
      ("Rem" (giac-insert "rem()"))
      ("R2e" (giac-insert "r2e()"))
      ("Ranm" (giac-insert "ranm()"))
      ("Tchebyshev1" (giac-insert "tchebyshev1()"))
      ("Tchebyshev2" (giac-insert "tchebyshev2()")))
  (-> "Calculus"
      ("Curl" (giac-insert "curl()"))
      ("Diff" (giac-insert "diff()"))
      ("Desolve" (giac-insert "desolve()"))
      ("Divergence" (giac-insert "divergence()"))
      ("Hessian" (giac-insert "hessian()"))
      ("Integrate" (giac-insert "integrate()"))
      ("Laplacian" (giac-insert "laplacian()"))
      ("Limit" (giac-insert "limit()"))
      ("Series" (giac-insert "series()"))
      ("Solve" (giac-insert "solve()"))
      ("Sum" (giac-insert "sum()")))
  (-> "Numeric analysis"
      ("Newton" (giac-insert "newton()"))
      ("Romberg" (giac-insert "romberg()"))
      ("Bisection" (giac-insert "fsolve(,bisection_solver)"))
      ("Brent" (giac-insert "fsolve(,brent_solver)"))
      ("Falsepos" (giac-insert "fsolve(,falsepos_solver)"))
      ("Newton" (giac-insert "fsolve(,newton_solver)"))
      ("Secant" (giac-insert "fsolve(,secant_solver)"))
      ("Newton D" (giac-insert "fsolve(,dnewton_solver)"))
      ("Hybrid" (giac-insert "fsolve(,hybrid_solver)"))
      ("Hybrid S" (giac-insert "fsolve(,hybrids_solver)"))
      ("Newton Jacobian" (giac-insert "fsolve(,newtonj_solver)"))
      ("Hybrid Jacobian" (giac-insert "fsolve(,hybridj_solver)"))
      ("Hybrid S Jacobian" (giac-insert "fsolve(,hybridsj_solver)")))
  (-> "Transformations"
      ("Fourier_an" (giac-insert "fourier_an()"))
      ("Fourier_bn" (giac-insert "fourier_bn()"))
      ("Fourier_cn" (giac-insert "fourier_cn()"))
      ("Inverse laplace" (giac-insert "ilaplace()"))
      ("Laplace" (giac-insert "laplace()")))
  (-> "Rewriting"
      ("Hyp2exp" (giac-insert "hyp2exp()"))
      ("Lin" (giac-insert "lin()"))
      ("Lncollect" (giac-insert "lncollect()"))
      ("Sincos" (giac-insert "sincos()"))
      ("Trig2exp" (giac-insert "trig2exp()"))
      ("Tsimplify" (giac-insert "tsimplify()")))
  (-> "Trigonometric rewriting"
      ("Halftan" (giac-insert "halftan()"))
      ("Tan2sincos" (giac-insert "tan2sincos()"))
      ("Tan2sincos2" (giac-insert "tan2sincos2()"))
      ("Tcollect" (giac-insert "tcollect()"))
      ("Texpand" (giac-insert "texpand()"))
      ("Trigcos" (giac-insert "trigcos()"))
      ("Trigsin" (giac-insert "trigsin()"))
      ("Trigtan" (giac-insert "trigtan()"))
      ("Tlin" (giac-insert "tlin()")))
  (-> "Inverse trigonometric rewriting"
      ("Acos2asin" (giac-insert "acos2asin()"))
      ("Acos2atan" (giac-insert "acos2atan()"))
      ("Asin2acos" (giac-insert "asin2acos()"))
      ("Asin2atan" (giac-insert "asin2atan()"))
      ("Atan2acos" (giac-insert "atan2acos()"))
      ("Atan2asin" (giac-insert "atan2asin()")))
  ---
  (-> "Vector"
      ("Abs" (giac-insert "abs()"))
      ("Scalar product" (giac-insert "dot()"))
      ("Vector product" (giac-insert "cross()")))
  (-> "Matrix"
      ("Hadamard" (giac-insert "hadamard()"))
      ("Identity" (giac-insert "idn()"))
      ("Inverse" (giac-insert "inv()"))
      ("Random" (giac-insert "ranm()"))
      ("Trace" (giac-insert "trace()"))
      ("Transconjugate" (giac-insert "trn()"))
      ("Transpose" (giac-insert "tran()"))
      ("Vandermonde" (giac-insert "vandermonde()")))
  (-> "Eigenvalues and vectors"
      ("Eigenvectors" (giac-insert "egv()"))
      ("Eigenvalues" (giac-insert "egvl()"))
      ("Jordan" (giac-insert "jordan()")))
  (-> "Matrix factorization"
      ("Cholesky" (giac-insert "cholesky()"))
      ("Lu" (giac-insert "lu()"))
      ("Qr" (giac-insert "qr()"))
      ("Svd" (giac-insert "svd()")))
  (-> "Quadratic forms"
      ("Axq" (giac-insert "axq()"))
      ("Gauss" (giac-insert "gauss()"))
      ("Qxa" (giac-insert "qxa()")))
  (-> "Gauss-Jordan pivot"
      ("Basis" (giac-insert "basis()"))
      ("Basis intersection" (giac-insert "ibasis()"))
      ("Ker" (giac-insert "ker()"))
      ("Image" (giac-insert "image()"))
      ("Linsolve" (giac-insert "linsolve()"))
      ("Rref" (giac-insert "rref()")))
  (-> "Isometries"
      ("Isom" (giac-insert "isom()"))
      ("Mkisom" (giac-insert "mkisom()"))))

(menu-bind plugin-menu
  (:require (in-giac?))
  (=> "Giac" (link giac-functions-menu)))
