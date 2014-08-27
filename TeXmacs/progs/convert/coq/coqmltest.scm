
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqmltest
;; DESCRIPTION : Test CoqML converters
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq coqmltest)
  (:use (convert coq coqmlscm)
        (convert coq scmcoqml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idempotence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define idempotence-test-suite
  '(;; strings
    ""
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

    "Eval compute in\n  let f := fun a b:nat =<gtr> (a+b) in\n    f 1 2."

    ;; bools
    #t
    #f

    ;; ints
    0
    1
    -1
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
    (call "SetOptions" ((pair ("key1" "val1") (option-value #t))
                        (pair ("key2" "val2") (option-value #f))
                        (pair ("key2" "val2") (option-value "opt"))
                        (pair ("key4" "val4") (option-value 1))))
    (option 1)
    (option #t)
    (option "opt")

    (state-id 1)))

(define (test-coqml-idempotence)
  (letrec ((test (lambda (st) (== st (coqml->stree (stree->coqml st)))))
           (proc (lambda (st)
                   (let ((test_verb `(tt ,(object->string st)))
                         (msg   (if (test st) "test passed: " "test failed: "))
                         (color (if (test st) "dark green" "dark red")))
                     `(concat (with "color" ,color ,msg) ,test_verb)))))
    `(document
       (strong "Idempotence testing: stree->coqml->stree")
       ,@(map proc idempotence-test-suite))))

(define (test-coqml-idempotence*)
  (letrec ((test (lambda (st)
                   (with xml (stree->coqml st)
                     (== xml (stree->coqml (coqml->stree xml))))))
           (proc (lambda (st)
                   (let ((test_verb `(tt ,(object->string st)))
                         (msg   (if (test st) "test passed: " "test failed: "))
                         (color (if (test st) "dark green" "dark red")))
                     `(concat (with "color" ,color ,msg) ,test_verb)))))
    `(document
       (strong "Idempotence testing: coqml->stree->coqml")
       ,@(map proc idempotence-test-suite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-coqml)
  (stree->tree
    `(document
       ,(test-coqml-idempotence)
       ""
       ,(test-coqml-idempotence*))))
