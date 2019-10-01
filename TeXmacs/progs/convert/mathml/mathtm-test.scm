
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathtm-test.scm
;; DESCRIPTION : Test suite for mathtm
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml mathtm-test)
  (:use (convert html htmltm)))

(tm-define (regtest-mathtm)
  (define (math->tree x) (htmltm-as-serial (cons 'math x)))
  (regression-test-group
   "mathtm" "mathtm"
   math->tree :none 
   (test "identifier" '((mi "x")) "x")
   (test "operator" '((mi "x") (mo "+") (mi "y")) "x+y")
   (test "numeral" '((mn "2") (mo "+") (mi "x")) "2+x")
   ;; (test "exponent" '(msup (mi "x") (mn "2")) '(concat "x" (rsup "2")))
   ;; (test "special ops"
   ;;   '(mrow (mn "3") (mo "&InvisibleTimes;") (mi "x")
   ;;      (mo "*") (msup (mi "y") (mn "4")))
   ;;   '(concat "3*x<ast>y" (rsup "4")))   
))
