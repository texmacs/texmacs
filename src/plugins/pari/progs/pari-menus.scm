
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
	(doc help-funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Pari expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-ref t . l)
  ;; FIXME: this routine should go into the standard library
  (and (tm? t)
       (with r (select t l)
	 (and (nnull? r) (car r)))))

(tm-define (plugin-output-simplify name t)
  (:require (== name "pari"))
  (cond ((match? t '(with "color" "magenta"
		      (concat :1 (with "mode" "math" "color" "blue" :1))))
	 `(math ,(plugin-output-simplify name (tm-ref t 2 1 4))))
	(else (plugin-output-std-simplify name t))))

(define pari-evaluable? plugin-evaluable?)
(define pari-evaluate plugin-evaluate)
(define pari-apply plugin-apply-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Pari menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind pari-menu
  (if (test-env? "prog-scripts" "pari")
      (when (pari-evaluable?)
	    ("Evaluate" (pari-evaluate)))
      ---)
  (-> "Elementary functions"
      ("Exponential" (pari-apply "exp"))
      ("Logarithm" (pari-apply "log"))
      ---
      ("Sine" (pari-apply "sin"))
      ("Cosine" (pari-apply "cos"))
      ("Tangent" (pari-apply "tan"))
      ("Arc sine" (pari-apply "asin"))
      ("Arc cosine" (pari-apply "acos"))
      ("Arc tangent" (pari-apply "atan"))))
