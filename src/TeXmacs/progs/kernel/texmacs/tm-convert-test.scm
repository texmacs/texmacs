
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-convert-test.scm
;; DESCRIPTION : Test suite for tm-convert
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(texmacs-module (kernel texmacs tm-convert-test)
  (:use (kernel texmacs tm-define)))

(define (regtest-format?)
  (regression-test-group
   "format?" "boolean"
   format? :none
   (test "python format" "python" #t)
   (test "scala format" "scala" #t)
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-get-name)
  (regression-test-group
   "format-get-name" "string"
   format-get-name :none
   (test "python format" "python" "Python Source Code")
   (test "scala format" "scala" "Scala Source Code")
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-from-suffix)
  (regression-test-group
   "format-from-suffix" "string"
   format-from-suffix :none
   (test "scheme format" "scm" "scheme")
   (test "python format" "py" "python")
   (test "java format" "java" "java")
   (test "scala format" "scala" "scala")
   (test "julia format" "jl" "julia")
   (test "cpp format" "cpp" "cpp")
   (test "cpp format" "hpp" "cpp")
   (test "cpp format" "cc" "cpp")
   (test "cpp format" "hh" "cpp")
   (test "mathemagix format" "mmx" "mathemagix")
   (test "mathemagix format" "mmh" "mathemagix")
   (test "scilab format" "sci" "scilab")
   (test "scilab format" "sce" "scilab")
   (test "texmacs format" "tm" "texmacs")
   (test "texmacs format" "ts" "texmacs")
   (test "texmacs format" "tmml" "tmml")
   (test "texmacs format" "stm" "stm")
   (test "png format" "png" "png")
   (test "no such format" "no-such-format" "generic")))

(tm-define (regtest-tm-convert)
  (let ((n (+ (regtest-format?)
              (regtest-format-get-name)
              (regtest-format-from-suffix))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-convert: ok\n")))
