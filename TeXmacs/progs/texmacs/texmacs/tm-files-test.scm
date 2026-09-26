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
  (stree->tree '(document "abc" (strong "de"))))

(define (valid-in-test-doc p)
  (cursor-memory-valid? (test-doc) p))

(define (subtree-in-test-doc p)
  (with t (cursor-memory-subtree (test-doc) p)
    (and t (tree->stree t))))

(define (regtest-cursor-memory-valid)
  (regression-test-group
   "valid cursor paths" "cursor-memory-valid"
   valid-in-test-doc :none
   (test "inside a string" '(0 2) #t)
   (test "end of a string" '(0 3) #t)
   (test "beyond a string" '(0 4) #f)
   (test "inside a nested string" '(1 0 1) #t)
   (test "after a compound tree" '(1 1) #t)
   (test "invalid compound position" '(1 2) #f)
   (test "missing paragraph" '(2 0) #f)
   (test "negative index" '(-1 0) #f)
   (test "empty path" '() #f)))

(define (regtest-cursor-memory-subtree)
  (regression-test-group
   "subtrees along outdated paths" "cursor-memory-subtree"
   subtree-in-test-doc :none
   (test "outdated string position" '(0 9) "abc")
   (test "outdated nested position" '(1 0 7) "de")
   (test "missing paragraph" '(5 0) #f)
   (test "empty path" '() #f)))

(tm-define (regtest-cursor-memory)
  (let ((n (+ (regtest-cursor-memory-valid)
              (regtest-cursor-memory-subtree))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of cursor-memory: ok\n")))
