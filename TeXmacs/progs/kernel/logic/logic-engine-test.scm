
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-engine-test.scm
;; DESCRIPTION : Test suite for the logic programming engine
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-engine-test)
  (:use (kernel logic logic-bind) (kernel logic logic-unify)
        (kernel logic logic-rules) (kernel logic logic-query)
        (kernel logic logic-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table logic-test-color%
  (red "#ff0000")
  (green "#00ff00")
  ((:or blue azure) "#0000ff"))

(define (logic-test-handler-a l) (cons 'a l))
(define (logic-test-handler-b l) (cons 'b l))

(logic-dispatcher logic-test-dispatch%
  (tag-a logic-test-handler-a)
  (tag-b logic-test-handler-b))

(logic-group logic-test-fruit% apple pear)

(logic-rules
  ((logic-test-father% "Abe" "Homer"))
  ((logic-test-father% "Homer" "Bart"))
  ((logic-test-father% "Homer" "Lisa"))
  ((logic-test-grandfather% 'x 'z)
   (logic-test-father% 'x 'y) (logic-test-father% 'y 'z)))

(define (logic-test-dispatch x)
  ;; same shape as tmhtml-dispatch: the table is passed at run time
  (let ((h 'logic-test-dispatch%))
    (with f (logic-ref ,h (car x))
      (and f (f (cdr x))))))

(define (sorted l) (sort l (lambda (x y) (string<? (object->string x)
                                                     (object->string y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-logic-basics)
  (regression-test-group
   "logic, variables and unification" "unify"
   :none :none
   (test "free-variable?" (list (free-variable? ''x) (free-variable? 'x)) '(#t #f))
   (test "unify with a free variable"
         (logic-unify '(f 'x) '(f 1)) '(((x . 1))))
   (test "unify two constants" (logic-unify '(f 1) '(f 1)) '(()))
   (test "unify, mismatch" (logic-unify '(f 1) '(f 2)) #f)))

(define (regtest-logic-tables)
  (regression-test-group
   "logic, tables, dispatchers and groups" "tables"
   :none :none
   (test "logic-ref" (logic-ref logic-test-color% 'red) "#ff0000")
   (test "logic-ref, missing key" (logic-ref logic-test-color% 'pink) #f)
   (test "logic-ref, :or key"
         (list (logic-ref logic-test-color% 'blue)
               (logic-ref logic-test-color% 'azure))
         '("#0000ff" "#0000ff"))
   (test "logic-ref with a run-time table name"
         (let ((h 'logic-test-color%)) (logic-ref ,h 'green)) "#00ff00")
   (test "logic-dispatcher" (logic-test-dispatch '(tag-a 1 2)) '(a 1 2))
   (test "logic-dispatcher, other tag" (logic-test-dispatch '(tag-b)) '(b))
   (test "logic-dispatcher, unknown tag" (logic-test-dispatch '(tag-c)) #f)
   (test "logic-group membership"
         (list (logic-in? 'apple logic-test-fruit%)
               (logic-in? 'stone logic-test-fruit%))
         '(#t #f))
   (test "logic-in? is cached, also when false"
         (list (logic-in? 'stone logic-test-fruit%)
               (logic-in? 'stone logic-test-fruit%))
         '(#f #f))))

(define (regtest-logic-rules)
  (regression-test-group
   "logic, rules and queries" "rules"
   :none :none
   (test "query a fact"
         (query '(logic-test-father% "Abe" "Homer")) '(()))
   (test "query a false fact" (query '(logic-test-father% "Bart" "Abe")) '())
   (test "query with a free variable"
         (sorted (query '(logic-test-father% "Homer" 'c)))
         '(((c . "Bart")) ((c . "Lisa"))))
   (test "derived rule"
         (sorted (query '(logic-test-grandfather% "Abe" 'g)))
         '(((g . "Bart")) ((g . "Lisa"))))
   (test "derived rule, inverse direction"
         (query '(logic-test-grandfather% 'g "Lisa"))
         '(((g . "Abe"))))
   (test "logic-query macro"
         (logic-query (logic-test-father% 'p "Homer"))
         '(((p . "Abe"))))
   (test "adding rules changes logic-rules-version"
         (let ((before (logic-rules-version)))
           (logic-rules ((logic-test-father% "Bart" "Maggie-junior")))
           (list (> (logic-rules-version) before)
                 (pair? (query '(logic-test-father% "Bart" "Maggie-junior")))))
         '(#t #t))
   (test "queries do not change logic-rules-version"
         (let ((before (logic-rules-version)))
           (query '(logic-test-father% "Homer" 'c))
           (= (logic-rules-version) before))
         #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-logic)
  (let ((n (+ (regtest-logic-basics)
              (regtest-logic-tables)
              (regtest-logic-rules))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of logic: ok\n")))
