
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqtopmltest
;; DESCRIPTION : Test CoqTopMl converters
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml coqtopmltest)
  (:use (convert coqml coqtopmlscm)
        (convert coqml scmcoqtopml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idempotence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define idempotence-test-suite
  '(;; strings
    "a"
    "z"
    "\""
    "'"
    "`"
    "&"
    "\\"
    "<less>"
    "<gtr>"
    "|"
    "/"
    "[] {} () a-z 0-9 \" ' ` & \\ <less> <gtr> | /"

    ;; bools
    #t
    #f

    ;; ints
    0
    1
    12345678912345678901234567890

    ;; lists
    ()
    (1)
    (1 1)
    (1 2 1)
    (1 3 3 1)
    (1 4 6 4 1)
    (1 5 10 10 5 1)
    (()(()(()(()(()(()(()(()(()(()(()(()()())())())())())())())())())())())())

    ;; compounds
    (unit)
    (call "Init" (unit))
    (call "SetOptions" ((pair ("key1" "val1") (option #t))
                        (pair ("key2" "val2") (option #f))
                        (pair ("key2" "val2") (option "opt"))
                        (pair ("key3" "val3") (option 1))))
    (state-id 1)))

(define (test-coqtopml-idempotence)
  (letrec ((test (lambda (st) (== st (coqtopml->stree (stree->coqtopml st)))))
           (proc (lambda (st)
                   (let ((test_verb `(tt ,(object->string st)))
                         (msg   (if (test st) "test passed: " "test failed: "))
                         (color (if (test st) "dark green" "dark red")))
                     `(concat (with "color" ,color ,msg) ,test_verb)))))
    (stree->tree `(document ,@(map proc idempotence-test-suite)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-coqtopml)
  (test-coqtopml-idempotence))
