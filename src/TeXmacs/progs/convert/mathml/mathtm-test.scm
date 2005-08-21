
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathtm-test.scm
;; DESCRIPTION : Test suite for mathtm
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert mathml mathtm-test)
  (:use (convert html htmltm)))

(tm-define (regtest-mathtm)
  (define (with-math x) `(with "mode" "math" ,x))
  (define (math->tree x) (htmltm-as-serial (cons 'math x)))
  (regression-test-group
   "mathtm" "mathtm"
   mathtm with-math
   (test "identifier" '((mi "x")) "x")
   (test "operator" '((mi "x") (mo "+") (mi "y")) "x+y")
   (test "numeral" '((mn "2") (mo "+") (mi "x")) "2+x")
   (test "exponent" '(msup (mi "x") (mn "2")) '(concat "x" (rsup "2")))
   (test "special ops"
	 '(mrow (mn "3") (mo "&InvisibleTimes;") (mi "x")
		(mo "*") (msup (mi "y") (mn "4")))
	 '(concat "3*x<ast>y" (rsup "4")))   
))
