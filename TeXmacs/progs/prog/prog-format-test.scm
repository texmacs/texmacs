
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-format-test.scm
;; DESCRIPTION : Test suite for prog format
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-format-test)
  (:use (prog prog-format-test)))

(define (regtest-cpp)
  (regression-test-group
   "cpp file suffix" "cpp format"
   format-from-suffix :none
   (test "*.cpp" "cpp" "cpp")
   (test "*.hh" "hh" "cpp")
   (test "*.hpp" "hpp" "cpp")
   (test "*.cc" "cc" "cpp")))

(define (regtest-scheme)
  (regression-test-group
   "scheme file suffix" "scheme format"
   format-from-suffix :none
   (test "*.scm" "scm" "scheme")))

(define (regtest-scala)
  (regression-test-group
   "scala file suffix" "scala format"
   format-from-suffix :none
   (test "*.scala" "scala" "scala")))

(define (regtest-java)
  (regression-test-group
   "java file suffix" "java format"
   format-from-suffix :none
   (test "*.java" "java" "java")))

(define (regtest-python)
  (regression-test-group
   "python file suffix" "python format"
   format-from-suffix :none
   (test "*.py" "py" "python")))


(tm-define (regtest-prog-format)
  (let ((n (+ (regtest-cpp)
              (regtest-java)
              (regtest-python)
              (regtest-scala)
              (regtest-scheme))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of prog-format: ok\n")))
