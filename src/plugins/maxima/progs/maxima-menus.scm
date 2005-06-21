
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

(tm-define (plugin-output-simplify name t)
  (:require (== name "maxima"))
  (cond ((match? t '(concat
		     (with "mode" "text" "font-family" "tt" "color" "red" :*) 
		     :*))
	 (plugin-output-simplify name `(concat ,@(cddr t))))
	(else (plugin-output-std-simplify name t))))

(define maxima-evaluate plugin-evaluate)
(define maxima-apply plugin-apply-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Maxima menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind maxima-menu
  (if (test-env? "prog-scripts" "maxima")
      (when (selection-active-any?)
	    ("Evaluate" (maxima-evaluate)))
      ---)
  (-> "Simplification"
      ("Factor" (maxima-apply "factor"))
      ("Expand" (maxima-apply "expand"))
      ---
      ("Contract logarithms" (maxima-apply "logcontract"))
      ("Expand logarithms" (maxima-apply "logexpand"))
      ---
      ("Contract trigonometric functions" (maxima-apply "trigreduce"))
      ("Expand trigonometric functions" (maxima-apply "trigexpand")))
  (-> "Elementary functions"
      ("Exponential" (maxima-apply "exp"))
      ("Logarithm" (maxima-apply "log"))
      ---
      ("Sine" (maxima-apply "sin"))
      ("Cosine" (maxima-apply "cos"))
      ("Tangent" (maxima-apply "tan"))
      ("Arc sine" (maxima-apply "asin"))
      ("Arc cosine" (maxima-apply "acos"))
      ("Arc tangent" (maxima-apply "atan")))
  (-> "Special functions"
      ("Erf" (maxima-apply "erf"))
      ("Gamma" (maxima-apply "Gamma"))))

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
