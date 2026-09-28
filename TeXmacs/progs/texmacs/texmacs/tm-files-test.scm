;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-files-test.scm
;; DESCRIPTION : Test suite for remembering cursor positions in files
;; COPYRIGHT   : (C) 2026  Jacopo Rizzuto
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-files-test)
  (:use (texmacs texmacs tm-files)))

(define (test-doc)
  (stree->tree '(document "abc" (strong "de")
                          (with "font-series" "bold" "xyz")
                          (folded "Sum" "hidden"))))

(define (target-in-test-doc p)
  (cursor-memory-target (test-doc) p))

(define (regtest-cursor-memory-target)
  (regression-test-group
   "restoring saved cursor paths" "cursor-memory-target"
   target-in-test-doc :none
   (test "inside a string" '(0 2) '(0 2))
   (test "inside a nested string" '(1 0 1) '(1 0 1))
   (test "after a compound tree" '(1 1) '(1 1))
   (test "inside the body of a with" '(2 2 1) '(2 2 1))
   (test "shortened string" '(0 9) '(0 3))
   (test "shortened nested string" '(1 0 7) '(1 0 2))
   (test "string replaced a compound" '(1 0 1 4) '(1 0 0))
   (test "attribute of a with" '(2 0 2) '(2 2 0))
   (test "hidden body of a fold" '(3 1 2) '(3 0 0))
   (test "missing child" '(1 5) '(1 0 0))
   (test "missing paragraph" '(5 0) '(3 0))
   (test "negative index" '(-1 0) '(0 0))
   (test "empty path" '() '(0 0))))

(tm-define (regtest-cursor-memory)
  (let ((n (regtest-cursor-memory-target)))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of cursor-memory: ok\n")))
