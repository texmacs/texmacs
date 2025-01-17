
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cite-sort-test.scm
;; DESCRIPTION : Test suite for cite-sort package
;; COPYRIGHT   : (C) 2023  jingkaimori
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils cite cite-sort-test)
  (:use (utils cite cite-sort)))

(define (regtest-indice-sort)
  (regression-test-group
   "test indice sorting and merging" "indice-sort"
   indice-sort :none 
   (test "unmerged two element"
     '(("1" (concat (write "bib1") (reference "bib1")))
       ("2" (concat (write "bib2") (reference "bib2")))
       ("4" (concat (write "bib4") (reference "bib4")))
       ("5" (concat (write "bib5") (reference "bib5"))))
     '((concat (write "bib1") (reference "bib1"))
       (concat (write "bib2") (reference "bib2"))
       (concat (write "bib4") (reference "bib4"))
       (concat (write "bib5") (reference "bib5"))))
   (test "unmerged one element"
     '(("1" (concat (write "bib1") (reference "bib1")))
       ("3" (concat (write "bib3") (reference "bib3"))))
     '((concat (write "bib1") (reference "bib1"))
       (concat (write "bib3") (reference "bib3"))))
   (test "discontinue merge"
     '(("1" (concat (write "bib1") (reference "bib1")))
       ("3" (concat (write "bib3") (reference "bib3")))
       ("4" (concat (write "bib4") (reference "bib4")))
       ("5" (concat (write "bib5") (reference "bib5")))
       ("7" (concat (write "bib7") (reference "bib7"))))
     '((concat (write "bib1") (reference "bib1"))
       (concat
         (write "bib3")
         (write "bib4")
         (write "bib5")
         (reference "bib3")
         ""
         (reference "bib5"))
       (concat (write "bib7") (reference "bib7"))))
   (test "merge at lease three elements"
     '(("1" (concat (write "bib1") (reference "bib1")))
       ("2" (concat (write "bib2") (reference "bib2")))
       ("3" (concat (write "bib3") (reference "bib3"))))
     '((concat
         (write "bib1")
         (write "bib2")
         (write "bib3")
         (reference "bib1")
         ""
         (reference "bib3"))))
   (test "merge five elements"
     '(("1" (concat (write "bib1") (reference "bib1")))
       ("3" (concat (write "bib3") (reference "bib3")))
       ("4" (concat (write "bib4") (reference "bib4")))
       ("2" (concat (write "bib2") (reference "bib2")))
       ("5" (concat (write "bib5") (reference "bib5"))))
     '((concat
         (write "bib1")
         (write "bib2")
         (write "bib3")
         (write "bib4")
         (write "bib5")
         (reference "bib1")
         ""
         (reference "bib5"))))
))

(tm-define (regtest-cite-sort)
  (let ((n (+ (regtest-indice-sort))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of cite-sort: ok\n")))
