
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : maxima-menus.scm
;; DESCRIPTION : Menus for the maxima plugin
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (maxima-menus)
  (:use (utils plugins plugin-cmd)
	(doc help-funcs)
	(dynamic scripts-edit)
        (dynamic session-menu)
	(convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Maxima expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maxima-spaces? t)
  (and (string? t) (in? t (list "" " " "  " "   " "    "))))

(define (maxima-prompt? t)
  (or (match? t '(text (with "font-family" "tt" "color" "red" :*)))
      (match? t '(with "font-family" "tt" "color" "red" :*))
      (match? t '(with "mode" "text" "font-family" "tt" "color" "red" :*))))

(define (maxima-output-simplify t)
  ;;(display* "Simplify " t "\n")
  (cond ((and (func? t 'concat) (> (length t) 2) (maxima-prompt? (cadr t)))
	 (plugin-output-std-simplify "maxima" (cons 'concat (cddr t))))
	((match? t '(with "math-display" "true" :%1))
	 (maxima-output-simplify (cAr t)))
	((match? t '(with "mode" "math" "math-display" "true" :%1))
	 `(math ,(maxima-output-simplify (cAr t))))
	((func? t 'text 1)
	 `(text ,(maxima-output-simplify (cAr t))))
	((func? t 'math 1)
	 `(math ,(maxima-output-simplify (cAr t))))
	((func? t 'with 1)
	 (maxima-output-simplify (cAr t)))
	((func? t 'with)
	 (rcons (cDr t) (maxima-output-simplify (cAr t))))
        ((and (func? t 'concat) (pair? (cdr t)) (maxima-spaces? (cadr t)))
         (maxima-output-simplify (cons (car t) (cddr t))))
	((func? t 'concat)
	 (apply tmconcat (map maxima-output-simplify (cdr t))))
	(else (plugin-output-std-simplify "maxima" t))))

(define (maxima-contains-prompt? t)
  (cond ((maxima-prompt? t) #t)
	((func? t 'concat)
	 (list-or (map maxima-contains-prompt? (cdr t))))
	((and (func? t 'with) (nnull? (cdr t)))
	 (maxima-contains-prompt? (cAr t)))
	((or (func? t 'text 1) (func? t 'math 1))
	 (maxima-contains-prompt? (cAr t)))
	(else #f)))

(tm-define (plugin-output-simplify name t)
  (:require (== name "maxima"))
  ;;(display* "Simplify output " t "\n")
  (if (func? t 'document)
      (with u (list-find (cdr t) maxima-contains-prompt?)
	(if u (maxima-output-simplify u) (maxima-output-simplify t)))
      (maxima-output-simplify t)))

(define maxima-apply script-apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Maxima menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind maxima-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Simplification"
      ("Simplify" (maxima-apply "fullratsimp"))
      ("Factor" (maxima-apply "factor"))
      ("Expand" (maxima-apply "expand"))
      ((eval '(concat "Expand " "w.r.t."))
       (maxima-apply "expandwrt" 2)))
  (-> "Solving equations"
      ("Solve" (maxima-apply "solve"))
      ("Solve in" (maxima-apply "solve" 2)))
  (-> "Arithmetic"
      ("Factor" (maxima-apply "factor"))
      ("Gcd" (maxima-apply "gcd"))
      ("Lcm" (maxima-apply "lcm")))
  (-> "Logarithms"
      ("Exponential" (maxima-apply "exp"))
      ("Logarithm" (maxima-apply "log"))
      ("Square root" (maxima-apply "sqrt"))
      ---
      ("Contract logarithms" (maxima-apply "logcontract"))
      ("Expand logarithms" (maxima-apply "logexpand")))
  (-> "Trigonometry"
      ("Cosine" (maxima-apply "cos"))
      ("Sine" (maxima-apply "sin"))
      ("Tangent" (maxima-apply "tan"))
      ("Arc cosine" (maxima-apply "acos"))
      ("Arc sine" (maxima-apply "asin"))
      ("Arc tangent" (maxima-apply "atan"))
      ---
      ("Reduce trigonometric functions" (maxima-apply "trigreduce"))
      ((eval '(concat "Reduce trigonometric functions " "w.r.t."))
       (maxima-apply "trigreduce" 2))
      ("Expand trigonometric functions" (maxima-apply "trigexpand")))
  (-> "Special functions"
      ("Airy" (maxima-apply "Airy"))
      ("Erf" (maxima-apply "erf"))
      ("Gamma" (maxima-apply "Gamma"))
      ("Psi" (maxima-apply "Psi")))
  (-> "Calculus"
      ("Differentiate" (maxima-apply "diff" 2))
      ("Integrate" (maxima-apply "integrate" 2)))
  (-> "Linear algebra"
      ("Determinant" (maxima-apply "determinant"))
      ("Echelon" (maxima-apply "echelon"))
      ("Eigenvalues" (maxima-apply "eigenvalues"))
      ("Invert" (maxima-apply "invert"))
      ("Rank" (maxima-apply "rank"))
      ("Transpose" (maxima-apply "transpose"))
      ("Triangularize" (maxima-apply "triangularize")))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (car-or-false l)
  (if (null? l) #f
      (car l)))

(define (maxima-dirs)
  (if (os-mingw?)
      (map (lambda (x) (string-drop-right x 1))
           (string-split (var-eval-system "maxima.bat -d") #\newline))
      (string-split (var-eval-system "maxima -d") #\newline)))

(define (maxima-htmldir)
  (map (lambda (x) (string-drop x (string-length "maxima-htmldir=")))
   (filter (lambda (x) (string-starts? x "maxima-htmldir="))
           (maxima-dirs))))

(define (maxima-help) 
  (with htmldir (car-or-false (maxima-htmldir))
   (define (concat-html-path html)
     (string-append (string-append htmldir "/")
                    html))
   (car-or-false (filter url-exists?
                  (map concat-html-path
                   (list "maxima_toc.html" "maxima_0.html"))))))

(menu-bind maxima-help-icons
  (with help (maxima-help)
   (if (and (in-maxima?) help)
      /
      ((balloon (icon "tm_help.xpm") "Maxima manual")
       (load-buffer help)))))

(menu-bind session-help-icons
  (:require (and (in-maxima?) (in-session?)))
  (link maxima-help-icons))

(menu-bind plugin-menu
  (:require (or (in-maxima?) (and (not-in-session?) (maxima-scripts?))))
  (=> "Maxima" (link maxima-menu)))
