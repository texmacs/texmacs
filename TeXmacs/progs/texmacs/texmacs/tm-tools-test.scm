
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-tools-test.scm
;; DESCRIPTION : Test suite for tm-tools
;; COPYRIGHT   : (C) 2026  The TeXmacs contributors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-tools-test)
  (:use (texmacs texmacs tm-tools)))

(define (statistics-count-message* args)
  (apply statistics-count-message args))

(define (statistics-summary-message* args)
  (apply statistics-summary-message args))

(define (regtest-statistics-count-message)
  (regression-test-group
   "statistics-count-message" "string"
   statistics-count-message* :none
   (test "document character count"
         '("Document" "character" 7)
         "Document character count: 7")
   (test "selection word count"
         '("Selection" "word" 3)
         "Selection word count: 3")
   (test "document line count"
         '("Document" "line" 2)
         "Document line count: 2")))

(define (regtest-statistics-summary-message)
  (regression-test-group
   "statistics-summary-message" "string"
   statistics-summary-message* :none
   (test "document statistics summary"
         '("Document" 12 3 2)
         "Document statistics: 12 characters, 3 words, 2 lines")
   (test "selection statistics summary"
         '("Selection" 5 1 1)
         "Selection statistics: 5 characters, 1 words, 1 lines")))

(tm-define (regtest-tm-tools)
  (let ((n (+ (regtest-statistics-count-message)
              (regtest-statistics-summary-message))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-tools: ok\n")))
