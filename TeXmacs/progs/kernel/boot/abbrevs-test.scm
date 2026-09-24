
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrevs-test.scm
;; DESCRIPTION : Test suite for hash tables, programming constructs and
;;               SRFI macros of the kernel
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot abbrevs-test)
  (:use (kernel boot abbrevs) (kernel boot ahash-table) (kernel boot srfi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sorted-alist t)
  (sort (ahash-table->list t)
        (lambda (x y) (string<? (object->string (car x))
                                (object->string (car y))))))

(define (table . l)
  (list->ahash-table l))

(define (regtest-ahash-tables)
  (regression-test-group
   "abbrevs, adaptive hash tables" "ahash"
   :none :none
   (test "ahash-ref" (ahash-ref (table '(a . 1) '(b . 2)) 'b) 2)
   (test "ahash-ref, missing key" (ahash-ref (table '(a . 1)) 'z) #f)
   (test "ahash-size of an empty table" (ahash-size (make-ahash-table)) 0)
   (test "ahash-size" (ahash-size (table '(a . 1) '(b . 2) '(c . 3))) 3)
   (test "ahash-size after overwriting a key"
         (let ((t (table '(a . 1)))) (ahash-set! t 'a 2) (ahash-size t))
         1)
   (test "ahash-remove!"
         (let ((t (table '(a . 1) '(b . 2))))
           (ahash-remove! t 'a)
           (list (ahash-ref t 'a) (ahash-size t)))
         '(#f 1))
   (test "ahash-get-handle"
         (ahash-get-handle (table '(a . 1)) 'a) '(a . 1))
   (test "ahash-get-handle, missing key"
         (ahash-get-handle (table '(a . 1)) 'z) #f)
   (test "string keys" (ahash-ref (table '("x" . 1)) (string #\x)) 1)
   (test "list keys" (ahash-ref (table '((1 2) . ok)) (list 1 2)) 'ok)
   (test "ahash-table->list"
         (sorted-alist (table '(b . 2) '(a . 1))) '((a . 1) (b . 2)))
   (test "list->frequencies"
         (sorted-alist (list->frequencies '(x y x x))) '((x . 3) (y . 1)))
   (test "ahash-table-invert"
         (sorted-alist (ahash-table-invert (table '(a . x) '(b . y))))
         '((x . a) (y . b)))
   (test "ahash-table-append"
         (sorted-alist (ahash-table-append (table '(a . 1)) (table '(b . 2))))
         '((a . 1) (b . 2)))
   (test "ahash-table-difference"
         (sorted-alist (ahash-table-difference (table '(a . 1) '(b . 2))
                                               (table '(b . 0))))
         '((a . 1)))
   (test "ahash-table-map"
         (sorted-alist (ahash-table-map 1+ (table '(a . 1) '(b . 2))))
         '((a . 2) (b . 3)))
   (test "ahash-ref*" (ahash-ref* (table) 'a 'default) 'default)
   (test "ahash-table-select"
         (sorted-alist (ahash-table-select (table '(a . 1) '(b . 2)) '(b c)))
         '((b . 2)))
   (test "ahash-with"
         (let* ((t (table '(k . 1)))
                (inside (ahash-with t 'k 2 (ahash-ref t 'k))))
           (list inside (ahash-ref t 'k)))
         '(2 1))
   (test "ahash-with, key absent before"
         (let* ((t (table))
                (inside (ahash-with t 'k 2 (ahash-ref t 'k))))
           (list inside (ahash-ref t 'k)))
         '(2 #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define abbrevs-test-global 'initial)

(define (read-global) abbrevs-test-global)

(define (regtest-abbrevs-constructs)
  (regression-test-group
   "abbrevs, programming constructs" "constructs"
   :none :none
   (test "==" (== '(1 "a") (list 1 "a")) #t)
   (test "!=" (!= 1 2) #t)
   (test "in?" (list (in? 2 '(1 2)) (in? 3 '(1 2))) '(#t #f))
   (test "cons-new" (list (cons-new 1 '(1 2)) (cons-new 0 '(1 2)))
         '((1 2) (0 1 2)))
   (test "list-n?" (list (list-1? '(a)) (list-2? '(a b)) (list-3? '(a)))
         '(#t #t #f))
   (test "keyword conversions"
         (list (keyword->string :foo) (string->keyword "bar")
               (keyword->number :%3) (number->keyword 4))
         '("foo" :bar 3 :%4))
   (test "with" (with x 2 (* x x)) 4)
   (test "with, destructuring" (with (a b) (list 1 2) (+ a b)) 3)
   (test "with-define" (with-define (sq x) (* x x) (sq 5)) 25)
   (test "and-with" (list (and-with x 1 (+ x 1)) (and-with x #f 'no)) '(2 #f))
   (test "with-result"
         (let ((n 0)) (list (with-result 'r (set! n 1)) n)) '(r 1))
   (test "with-global sets the value during the body"
         (with-global abbrevs-test-global 'temporary (read-global))
         'temporary)
   (test "with-global restores the value"
         (begin (with-global abbrevs-test-global 'temporary 'ignored)
                abbrevs-test-global)
         'initial)
   (test "with-global keeps multiple values"
         (call-with-values
             (lambda () (with-global abbrevs-test-global 1 (values 2 3)))
           list)
         '(2 3))
   (test "for over a list"
         (let ((s 0)) (for (x '(1 2 3)) (set! s (+ s x))) s) 6)
   (test "for over a range" (let ((l '())) (for (i 0 3) (set! l (cons i l))) l)
         '(2 1 0))
   (test "for with a step" (let ((l '())) (for (i 0 6 2) (set! l (cons i l))) l)
         '(4 2 0))
   (test "for with a negative step"
         (let ((l '())) (for (i 3 0 -1) (set! l (cons i l))) l)
         '(1 2 3))
   (test "repeat" (let ((n 0)) (repeat 4 (set! n (+ n 1))) n) 4)
   (test "twice" (let ((n 0)) (twice (set! n (+ n 1))) n) 2)
   (test ".." (.. 0 4) '(0 1 2 3))
   (test "..." (... 0 4 2) '(0 2 4))
   (test "sourcify of a closure"
         (car (sourcify (lambda (x) (+ x 1)))) 'lambda)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRFI macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define arity-test
  (case-lambda
    (() 'none)
    ((x) (list 'one x))
    ((x y) (list 'two x y))
    ((x . rest) (list 'rest x rest))))

(define (regtest-srfi)
  (regression-test-group
   "abbrevs, SRFI macros" "srfi"
   :none :none
   (test "receive"
         (receive (a b . c) (values 1 2 3 4) (list a b c)) '(1 2 (3 4)))
   (test "receive, single value" (receive (a) (values 7) a) 7)
   (test "case-lambda, no argument" (arity-test) 'none)
   (test "case-lambda, one argument" (arity-test 1) '(one 1))
   (test "case-lambda, two arguments" (arity-test 1 2) '(two 1 2))
   (test "case-lambda, rest arguments" (arity-test 1 2 3) '(rest 1 (2 3)))
   (test "case-lambda, too few arguments"
         (catch #t (lambda () ((case-lambda ((x) x)))) (lambda args 'error))
         'error)
   (test "cut" ((cut list 1 <> 3) 2) '(1 2 3))
   (test "cut with rest" ((cut list 1 <...>) 2 3) '(1 2 3))
   (test "cute evaluates its arguments once"
         (let* ((n 0) (f (cute + (begin (set! n (+ n 1)) n) <>)))
           (f 10) (f 10) n)
         1)
   (test "and-let*" (and-let* ((x 1) ((> x 0)) (y 2)) (+ x y)) 3)
   (test "and-let*, failing test" (and-let* ((x 1) ((< x 0))) 'no) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-and-load value)
  (let ((u (url-temp)))
    (save-object u value)
    (with r (load-object u)
      (system-remove u)
      r)))

(define (regtest-save-object)
  (regression-test-group
   "abbrevs, saving and loading objects" "save-object"
   :none :none
   (test "list" (save-and-load '(1 "two" three (4 . 5))) '(1 "two" three (4 . 5)))
   (test "long list" (save-and-load (iota 1000)) (iota 1000))
   ;; s7 truncates long vectors when printing, unless print-length is raised
   (test "long vector" (save-and-load (make-vector 100 7)) (make-vector 100 7))
   (test "string with special characters"
         (save-and-load "a\"b\\c\nd") "a\"b\\c\nd")
   (test "missing file" (load-object (url-temp)) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-abbrevs)
  (let ((n (+ (regtest-ahash-tables)
              (regtest-abbrevs-constructs)
              (regtest-srfi)
              (regtest-save-object))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of abbrevs: ok\n")))
