
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat-s7-test.scm
;; DESCRIPTION : Test suite for the Guile compatibility layer on top of S7
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot compat-s7-test)
  (:use (kernel boot compat-s7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists and association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-compat-lists)
  (regression-test-group
   "compat-s7, lists" "lists"
   :none :none
   (test "filter" (filter odd? '(1 2 3 4 5)) '(1 3 5))
   (test "filter, nothing kept" (filter odd? '(2 4)) '())
   (test "delq" (delq 'a '(a b a c)) '(b c))
   (test "delq is not destructive"
         (let ((l (list 'a 'b))) (delq 'a l) l) '(a b))
   (test "acons" (acons 'k 1 '((j . 2))) '((k . 1) (j . 2)))
   (test "last-pair" (last-pair '(1 2 3)) '(3))
   (test "last-pair, dotted" (last-pair '(1 2 . 3)) '(2 . 3))
   (test "list-copy is a fresh list"
         (let* ((l (list 1 2 3)) (c (list-copy l)))
           (set-car! c 0) (list l c))
         '((1 2 3) (0 2 3)))
   (test "copy-tree is deep"
         (let* ((l (list (list 1 2) 3)) (c (copy-tree l)))
           (set-car! (car c) 0) (list l c))
         '(((1 2) 3) ((0 2) 3)))
   (test "map-in-order" (map-in-order + '(1 2) '(10 20)) '(11 22))
   (test "append!" (append! (list 1) (list 2 3)) '(1 2 3))
   (test "iota" (iota 5) '(0 1 2 3 4))
   (test "iota 0" (iota 0) '())
   (test "assoc-ref" (assoc-ref '(("a" . 1) ("b" . 2)) "b") 2)
   (test "assoc-ref, missing" (assoc-ref '(("a" . 1)) "z") #f)
   (test "assoc-set!, existing key"
         (assoc-set! (list (cons 'a 1)) 'a 2) '((a . 2)))
   (test "assoc-set!, new key"
         (assoc-set! (list (cons 'a 1)) 'b 2) '((b . 2) (a . 1)))
   (test "sort is not destructive"
         (let* ((l (list 3 1 2)) (s (sort l <))) (list l s))
         '((3 1 2) (1 2 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Guile builtins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-compat-misc)
  (regression-test-group
   "compat-s7, miscellaneous builtins" "misc"
   :none :none
   (test "1+" (1+ 41) 42)
   (test "1-" (1- 43) 42)
   (test "noop without arguments" (noop) #f)
   (test "noop returns its first argument" (noop 1 2) 1)
   (test "symbol-append" (symbol-append 'foo '- 'bar) 'foo-bar)
   (test "string-null?" (list (string-null? "") (string-null? "a")) '(#t #f))
   (test "symbol? rejects keywords" (symbol? :foo) #f)
   (test "symbol? accepts symbols" (symbol? 'foo) #t)
   (test "keyword? of a keyword" (keyword? :foo) #t)
   (test "keywords are self-evaluating" :foo (string->keyword "foo"))
   (test "list? rejects dotted lists" (list? '(1 . 2)) #f)
   (test "list? accepts proper lists" (list (list? '()) (list? '(1 2))) '(#t #t))
   (test "while with break"
         (let ((i 0)) (while #t (set! i (+ i 1)) (if (= i 5) (break))) i)
         5)
   (test "while with continue"
         (let ((i 0) (s 0))
           (while (< i 6)
             (set! i (+ i 1))
             (if (odd? i) (continue))
             (set! s (+ s i)))
           s)
         12)
   (test "while, false condition" (let ((n 0)) (while #f (set! n 1)) n) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-compat-strings)
  (regression-test-group
   "compat-s7, strings" "strings"
   :none :none
   (test "string-split" (string-split "a,b,c" #\,) '("a" "b" "c"))
   (test "string-split, empty fields"
         (string-split ",a,,b," #\,) '("" "a" "b" ""))
   (test "string-split, no separator" (string-split "abc" #\,) '("abc"))
   (test "string-split, empty string" (string-split "" #\,) '())
   (test "string-index, char" (string-index "hello" #\l) 2)
   (test "string-index, missing char" (string-index "hello" #\z) #f)
   (test "string-rindex, char" (string-rindex "hello" #\l) 3)
   (test "string-index, predicate" (string-index "ab1c" char-numeric?) 2)
   (test "string-rindex, predicate"
         (string-rindex "a1b2c" char-numeric?) 3)
   (test "string-index, char-set"
         (string-index "abc def" char-set:whitespace) 3)
   (test "string-rindex, complement of a char-set"
         (string-rindex "ab  " (char-set-complement char-set:whitespace)) 1)
   (test "string-index, empty string" (string-index "" #\a) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Char-sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (strong-password? p)
  ;; same computation as server-strong-password? in server-authentication
  (let ((c (string->char-set p)))
    (and (>= (string-length p) 10)
         (>= (char-set-size (char-set-intersection char-set:lower-case c)) 1)
         (>= (char-set-size (char-set-intersection char-set:upper-case c)) 1)
         (>= (char-set-size (char-set-intersection char-set:digit c)) 1)
         (>= (char-set-size (char-set-intersection
                             (string->char-set "!?#$%&*") c)) 1))))

(define (regtest-compat-char-sets)
  (regression-test-group
   "compat-s7, char-sets" "char-sets"
   :none :none
   (test "char-set-size of string->char-set, duplicates"
         (char-set-size (string->char-set "aabc")) 3)
   (test "char-set-size of an empty set" (char-set-size (char-set)) 0)
   (test "char-set-contains?"
         (list (char-set-contains? (char-set #\a #\b) #\a)
               (char-set-contains? (char-set #\a #\b) #\c))
         '(#t #f))
   (test "char-sets are applicable"
         (let ((cs (string->char-set "xy"))) (list (cs #\x) (cs #\z)))
         '(#t #f))
   (test "char-set-adjoin"
         (char-set-size (char-set-adjoin char-set:whitespace #\( #\))) 5)
   (test "char-set-adjoin does not change its argument"
         (let ((cs (char-set #\a))) (char-set-adjoin cs #\b) (char-set-size cs))
         1)
   (test "char-set-complement" (char-set-size (char-set-complement (char-set #\a)))
         255)
   (test "char-set-intersection"
         (char-set-size (char-set-intersection (string->char-set "abcd")
                                               (string->char-set "bcde")))
         3)
   (test "char-set-intersection with a predicate"
         (char-set-size (char-set-intersection char-numeric?
                                               (string->char-set "a1b2")))
         2)
   (test "char-set-union"
         (char-set-size (char-set-union (char-set #\a #\b) (string->char-set "bc")))
         3)
   (test "char-set:lower-case"
         (char-set-size (char-set-intersection char-set:lower-case
                                               (string->char-set "abcXYZ12!")))
         3)
   (test "char-set:upper-case"
         (char-set-size (char-set-intersection char-set:upper-case
                                               (string->char-set "abcXYZ12!")))
         3)
   (test "char-set:digit" (char-set-size char-set:digit) 10)
   (test "strong password" (strong-password? "Abcdefgh1!") #t)
   (test "password without upper case" (strong-password? "abcdefgh1!") #f)
   (test "password without symbol" (strong-password? "Abcdefgh1x") #f)
   (test "short password" (strong-password? "Ab1!") #f)
   ;; s7 used to mis-apply a closure called from a loop after the loop had
   ;; run with a closure of another shape; char-sets must not be affected
   (test "char-set-size after sizes of intersections"
         (begin
           (strong-password? "Abcdefgh1!")
           (char-set-size (string->char-set "abc")))
         3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records, promises, hashing and random numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define point-type (make-record-type "point" '(x y)))
(define make-point (record-constructor point-type))
(define point? (record-predicate point-type))
(define point-x (record-accessor point-type 'x))
(define point-y (record-accessor point-type 'y))
(define other-type (make-record-type "other" '(x)))
(define make-other (record-constructor other-type))

(define (random-list seed n)
  (set! *random-state* (seed->random-state seed))
  (map (lambda (i) (random 1000000)) (iota n)))

(define (regtest-compat-data)
  (regression-test-group
   "compat-s7, records, promises, hashing, random" "data"
   :none :none
   (test "record accessors"
         (let ((p (make-point 1 2))) (list (point-x p) (point-y p)))
         '(1 2))
   (test "record predicate"
         (list (point? (make-point 1 2)) (point? (make-other 1)))
         '(#t #f))
   (test "force of delay" (force (delay (+ 1 2))) 3)
   (test "delay is evaluated once"
         (let* ((n 0) (p (delay (begin (set! n (+ n 1)) n))))
           (force p) (force p) n)
         1)
   (test "delay-force" (force (delay-force (delay 5))) 5)
   (test "hash is stable" (== (hash "abc") (hash "abc")) #t)
   (test "hash with a bound" (< (hash "abc" 7) 7) #t)
   (test "setting *random-state* reseeds random"
         (== (random-list 17 5) (random-list 17 5))
         #t)
   (test "different seeds give different sequences"
         (== (random-list 17 5) (random-list 18 5))
         #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Curried define (only available in modules)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((adder a) b) (+ a b))
(define (((adder3 a) b) c) (+ a b c))
(define ((adder* a) . l) (apply + a l))

(define (regtest-compat-curried)
  (regression-test-group
   "compat-s7, curried define" "curried"
   :none :none
   (test "one level" ((adder 1) 2) 3)
   (test "two levels" (((adder3 1) 2) 3) 6)
   (test "rest arguments" ((adder* 1) 2 3 4) 10)
   (test "plain define still works" (let () (define x 5) x) 5)
   (test "internal curried define"
         (let () (define ((mul a) b) (* a b)) ((mul 6) 7))
         42)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-compat-s7)
  (let ((n (+ (regtest-compat-lists)
              (regtest-compat-misc)
              (regtest-compat-strings)
              (regtest-compat-char-sets)
              (regtest-compat-data)
              (regtest-compat-curried))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of compat-s7: ok\n")))
