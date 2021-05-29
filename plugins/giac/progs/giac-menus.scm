
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
      ("Plot function" (giac-insert "plotfunc()"))
      ("Plot parametric function" (giac-insert "plotparam()"))
      ("Polar plot" (giac-insert "plotpolar()"))
      ("Sequence plot" (giac-insert "plotseq()")))
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
      ("Absolute value" (giac-insert "abs()"))
      ("Factorial" (giac-insert "factorial()"))
      ("Maximal element" (giac-insert "max()"))
      ("Minimal element" (giac-insert "min()"))
      ("Sign function" (giac-insert "sign()")))
  (-> "Complex"
      ("Absolute value" (giac-insert "abs()"))
      ("Argumennt" (giac-insert "arg()"))
      ("Conjugate" (giac-insert "conj()"))
      ("Imaginary part" (giac-insert "im()"))
      ("Real part" (giac-insert "re()")))
  (-> "Exponential and logarithmic functions"
      ("Exponential function" (giac-insert "exp()"))
      ("Natural logarithm" (giac-insert "ln()"))
      ("Common logarithm (base 10)" (giac-insert "log10()")))
  (-> "Trigonometric and cyclometric functions"
      ("Sine" (giac-insert "sin()"))
      ("Cosine" (giac-insert "cos()"))
      ("Tangent" (giac-insert "tan()"))
      ("Cotangent" (giac-insert "cot()"))
      ("Arcsine" (giac-insert "asin()"))
      ("Arccosine" (giac-insert "acos()"))
      ("Arctangent" (giac-insert "atan()")))
  (-> "Hyperbolic functions"
      ("Hyperbolic sine" (giac-insert "sinh()"))
      ("Hyperbolic cosine" (giac-insert "cosh()"))
      ("Hyperbolic tangent" (giac-insert "tanh()"))
      ("Inverse hyperbolic sine" (giac-insert "asinh()"))
      ("Inverse hyperbolic cosine" (giac-insert "acosh()"))
      ("Inverse hyperbolic tangent" (giac-insert "atanh()")))
  (-> "Special functions"
      ("Ai (Airy function)" (giac-insert "Airy_Ai()"))
      ("Bi (Airy function" (giac-insert "Airy_Bi"))
      ("Gamma" (giac-insert "gamma()"))
      ("Psi" (giac-insert "psi()"))
      ("Zeta" (giac-insert "zeta()")))
  ---
  (-> "Integer arithmetic"
      ("Greatest common divisor" (giac-insert "gcd()"))
      ("Largest common multiple" (giac-insert "lcm()"))
      ("Euler totient function" (giac-insert "euler()"))
      ("Iabcuv" (giac-insert "iabcuv()"))
      ("Chinese remainders" (giac-insert "ichinrem()"))
      ("Extended greatest common divisor" (giac-insert "iegcd()"))
      ("Integer factorization" (giac-insert "ifactor()"))
      ("Prime factors" (giac-insert "ifactors()"))
      ("Quotient" (giac-insert "iquo()"))
      ("Remainder" (giac-insert "irem()"))
      ("Is prime" (giac-insert "is_prime()"))
      ("Next prime" (giac-insert "nextprime()"))
      ("Pa2b2" (giac-insert "pa2b2()"))
      ("Previous prime" (giac-insert "prevprime()")))
  (-> "Polynomial arithmetic"
      ("Greatest common divisor" (giac-insert "gcd()"))
      ("Largest common multiple" (giac-insert "lcm()"))
      ("Abcuv" (giac-insert "abcuv()"))
      ("Chinese remainders" (giac-insert "chinrem()"))
      ("Cyclotomic" (giac-insert "cyclotomic()"))
      ("Divisors of a polynomial" (giac-insert "divis()"))
      ("Extended greatest common divisor" (giac-insert "egcd()"))
      ("Factor" (giac-insert "factor()"))
      ("Hermite polynomial" (giac-insert "hermite()"))
      ("Laguerre polynomial" (giac-insert "laguerre()"))
      ("Coefficients from roots" (giac-insert "pcoeff()"))
      ("Evaluation from coefficients" (giac-insert "peval()"))
      ("Roots from coefficients" (giac-insert "proot()"))
      ("Quotient" (giac-insert "quo()"))
      ("Remainder" (giac-insert "rem()"))
      ("Quotient and remainder" (giac-insert "quorem()"))
      ("Coefficients" (giac-insert "e2r()"))
      ("Polynomial from coefficients" (giac-insert "r2e()"))
      ("Tchebyshev polynomial of the first kind" (giac-insert "tchebyshev1()"))
      ("Tchebyshev polynomial of the second kind" (giac-insert "tchebyshev2()")))
  (-> "Calculus"
      ("Differentiate" (giac-insert "diff()"))
      ("Integrate" (giac-insert "integrate()"))
      ("Solve (in)equation(s)" (giac-insert "solve()"))
      ("Solve ordinary differential equation" (giac-insert "desolve()"))
      ("Gradient" (giac-insert "grad()"))
      ("Divergence" (giac-insert "divergence()"))
      ("Curl" (giac-insert "curl()"))
      ("Hessian" (giac-insert "hessian()"))
      ("Laplacian" (giac-insert "laplacian()"))
      ("Limit" (giac-insert "limit()"))
      ("Taylor expansion" (giac-insert "taylor()"))
      ("Series expansion" (giac-insert "series()"))
      ("Sum" (giac-insert "sum()")))
  (-> "Numeric analysis"
      ("Newton method" (giac-insert "newton()"))
      ("Bisection solver" (giac-insert "fsolve(,bisection_solver)"))
      ("Brent solver" (giac-insert "fsolve(,brent_solver)"))
      ("False positions solver" (giac-insert "fsolve(,falsepos_solver)"))
      ("Newton solver" (giac-insert "fsolve(,newton_solver)"))
      ("Romberg" (giac-insert "romberg()"))
      ("Secant" (giac-insert "fsolve(,secant_solver)"))
      ("Newton D" (giac-insert "fsolve(,dnewton_solver)"))
      ("Hybrid" (giac-insert "fsolve(,hybrid_solver)"))
      ("Hybrid S" (giac-insert "fsolve(,hybrids_solver)"))
      ("Newton Jacobian" (giac-insert "fsolve(,newtonj_solver)"))
      ("Hybrid Jacobian" (giac-insert "fsolve(,hybridj_solver)"))
      ("Hybrid S Jacobian" (giac-insert "fsolve(,hybridsj_solver)")))
  (-> "Transformations"
      ("Fourier cosine coefficient" (giac-insert "fourier_an()"))
      ("Fourier sine coefficient" (giac-insert "fourier_bn()"))
      ("Fourier coefficient" (giac-insert "fourier_cn()"))
      ("Laplace transform" (giac-insert "laplace()"))
      ("Inverse Laplace transform" (giac-insert "ilaplace()"))
      ("Fourier transform" (giac-insert "fourier()"))
      ("Inverse Fourier transform" (giac-insert "ifourier()"))
      ("Fast Fourier Transform" (giac-insert "fft()"))
      ("Inverse Fast Fourier Transform" (giac-insert "ifft()")))
  (-> "Rewriting"
      ("Hyperbolic to exponential" (giac-insert "hyp2exp()"))
      ("Linearize exponentials" (giac-insert "lin()"))
      ("Collect logarithms" (giac-insert "lncollect()"))
      ("Complex exponential to sine and cosine" (giac-insert "sincos()"))
      ("Trigonometric expression to exponential" (giac-insert "trig2exp()"))
      ("Lower the number of non-rational variables" (giac-insert "tsimplify()")))
  (-> "Trigonometric rewriting"
      ("Halftan" (giac-insert "halftan()"))
      ("Tan2sincos" (giac-insert "tan2sincos()"))
      ("Tan2sincos2" (giac-insert "tan2sincos2()"))
      ("Collect transcendental expressions" (giac-insert "tcollect()"))
      ("Expand transcendental expressions" (giac-insert "texpand()"))
      ("Simplify by privileging sine" (giac-insert "trigsin()"))
      ("Simplify by privileging cosine" (giac-insert "trigcos()"))
      ("Simplify by privileging tangent" (giac-insert "trigtan()"))
      ("Trigonometric linearization" (giac-insert "tlin()")))
  (-> "Inverse trigonometric rewriting"
      ("Arccosine to arcsine" (giac-insert "acos2asin()"))
      ("Arccosine to arctangent" (giac-insert "acos2atan()"))
      ("Arcsine to arccosine" (giac-insert "asin2acos()"))
      ("Arcsine2 to arctangent" (giac-insert "asin2atan()"))
      ("Arctangent to arccosine" (giac-insert "atan2acos()"))
      ("Arctangent to arcsine" (giac-insert "atan2asin()")))
  ---
  (-> "Vector"
      ("Absolute value" (giac-insert "abs()"))
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
      ("Cholesky decomposition" (giac-insert "cholesky()"))
      ("LU decomposition" (giac-insert "lu()"))
      ("QR decomposition" (giac-insert "qr()"))
      ("Singular value decomposition" (giac-insert "svd()")))
  (-> "Gauss-Jordan pivot"
      ("Basis" (giac-insert "basis()"))
      ("Basis intersection" (giac-insert "ibasis()"))
      ("Kernel" (giac-insert "ker()"))
      ("Image" (giac-insert "image()"))
      ("Solve linear systems" (giac-insert "linsolve()"))
      ("Row reduction to echelon form" (giac-insert "rref()")))
  (-> "Isometries"
      ("Elements of a 2D or 3D isometry" (giac-insert "isom()"))
      ("Isometry matrix" (giac-insert "mkisom()"))))

(menu-bind plugin-menu
  (:require (in-giac?))
  (=> "Giac" (link giac-functions-menu)))
