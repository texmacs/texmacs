
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : maxima-menus.scm
;; DESCRIPTION : Menus for the maxima plugin
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (maxima-menus)
  (:use (utils plugins plugin-cmd)
	(doc help-funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Maxima expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maxima-output? t)
  (match? t
    '(concat (with "mode" "text" "font-family" "tt" "color" "red" :*) :*)))

(define (maxima-var-output? t)
  (or (maxima-output? t)
      (match? t '(concat (with "mode" "math" "math-display" "true" :*) :*))))

(tm-define (plugin-output-simplify name t)
  (:require (== name "maxima"))
  (cond ((func? t 'document)
	 (with u (list-find (cdr t) maxima-var-output?)
	   (if u (plugin-output-simplify name u) "")))
	((maxima-output? t)
	 (plugin-output-simplify name `(concat ,@(cddr t))))
	((match? t '(with "mode" "math" "math-display" "true" :1))
	 `(math ,(plugin-output-simplify name (list-ref t 5))))
	(else (plugin-output-std-simplify name t))))

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
      ("Expand w.r.t." (maxima-apply "expandwrt" 2)))
  (-> "Arithmetic"
      ("Factor" (maxima-apply "factor"))
      ("Gcd" (maxima-apply "gcd"))
      ("Lcm" (maxima-apply "lcm")))
  (-> "Logarithms"
      ("Exponential" (maxima-apply "exp"))
      ("Logarithm" (maxima-apply "log"))
      ---
      ("Contract logarithms" (maxima-apply "logcontract"))
      ("Expand logarithms" (maxima-apply "logexpand")))
  (-> "Trigonometric"
      ("Sine" (maxima-apply "sin"))
      ("Cosine" (maxima-apply "cos"))
      ("Tangent" (maxima-apply "tan"))
      ("Arc sine" (maxima-apply "asin"))
      ("Arc cosine" (maxima-apply "acos"))
      ("Arc tangent" (maxima-apply "atan"))
      ---
      ("Reduce trigonometric functions" (maxima-apply "trigreduce"))
      ("Reduce trigonometric functions w.r.t." (maxima-apply "trigreduce" 2))
      ("Expand trigonometric functions" (maxima-apply "trigexpand")))
  (-> "Special functions"
      ("Airy" (maxima-apply "Airy"))
      ("Erf" (maxima-apply "erf"))
      ("Gamma" (maxima-apply "Gamma"))
      ("Psi" (maxima-apply "Psi")))
  (-> "Calculus"
      ("Differentiate" (maxima-apply "diff" 2))
      ("Integrate" (maxima-apply "integrate" 2)))
  (-> "Matrices"
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

(menu-bind maxima-help-icons
  (if (and (in-maxima?)
	   (url-exists? "$TM_MAXIMA_HOME/info/maxima_toc.html"))
      |
      ((balloon (icon "tm_help.xpm") "Maxima manual")
       (load-help-buffer "$TM_MAXIMA_HOME/info/maxima_toc.html")))
  (if (and (in-maxima?)
	   (url-exists? "$TM_MAXIMA_HOME/doc/html/maxima_toc.html"))
      |
      ((balloon (icon "tm_help.xpm") "Maxima manual")
       (load-help-buffer "$TM_MAXIMA_HOME/doc/html/maxima_toc.html"))))
