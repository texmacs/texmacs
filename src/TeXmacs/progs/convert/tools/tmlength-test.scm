
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmlength-test.scm
;; DESCRIPTION : test suite for length library
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmlength-regtest)
  (:use (convert tools tmlength)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regtest routines for tmlength
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-string->tmlength)
  (define (list->tmlength l) (apply tmlength l))
  (regression-test-group
   "tmlength tools" "tmlength"
    string->tmlength list->tmlength
    (test "null length" "" '())
    (test "zero length" "0cm" '(0 cm))
    (test "implicit zero" "px" '(0 px))
    (test "decimal length" "123.456mm" '(123.456 mm))
    (test "negative length" "-1in" '(-1 in))
    (test "negative^2 length" "--2fns" '(2 fns))))

(tm-define (regtest-tmlength)
  (let ((n (+ (regtest-string->tmlength))))
    (display* "Total: " (number->string n) "tests.\n")
    (display "Test suite of tmlength: ok\n")))
