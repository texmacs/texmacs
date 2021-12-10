
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-define-test.scm
;; DESCRIPTION : Test suite for tm-define
;; COPYRIGHT   : (C) 2021  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-format-test)
  (:use (kernel texmacs tm-define)))

(define (regtest-procedure-name)
  (regression-test-group
   "procedure" "procedure"
   procedure-name :none
   (test "procedures defined via define-public" string->float string->float)
   (test "procedures defined via glue symbols" utf8->cork utf8->cork)
   (test "procedures defined via tm-define" regtest-tm-define regtest-tm-define)
   (test "invalid input" 1 #f)))

(tm-define (regtest-tm-define)
  (let ((n (+ (regtest-procedure-name))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-define: ok\n")))
