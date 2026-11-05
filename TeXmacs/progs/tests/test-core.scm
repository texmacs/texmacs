;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-core.scm
;; DESCRIPTION : Core Scheme and language unit tests
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-core)
  (:use (tests test-runner)))

(define-test "core: arithmetic and basic numbers"
  (assert-equal 4 (+ 2 2))
  (assert-equal 12 (* 3 4))
  (assert-equal -5 (- 5 10))
  (assert-equal 3 (/ 9 3))
  (assert-true (> 10 5))
  (assert-false (< 10 5))
  (assert-true (= 42 42)))

(define-test "core: boolean logic"
  (assert-true #t)
  (assert-false #f)
  (assert-true (and #t #t))
  (assert-false (and #t #f))
  (assert-true (or #f #t))
  (assert-false (or #f #f))
  (assert-true (not #f))
  (assert-false (not #t)))

(define-test "core: list operations"
  (assert-equal '() '())
  (assert-equal '(1 2 3) (list 1 2 3))
  (assert-equal '(0 1 2 3) (cons 0 '(1 2 3)))
  (assert-equal 1 (car '(1 2 3)))
  (assert-equal '(2 3) (cdr '(1 2 3)))
  (assert-equal 3 (length '(1 2 3)))
  (assert-equal '(3 2 1) (reverse '(1 2 3)))
  (assert-equal '(1 2 3 4 5) (append '(1 2) '(3 4 5)))
  (assert-equal '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
  (assert-equal '(2 4) (filter even? '(1 2 3 4 5))))

(define-test "core: string manipulation"
  (assert-equal "hello world" (string-append "hello " "world"))
  (assert-equal 5 (string-length "hello"))
  (assert-equal "ell" (substring "hello" 1 4))
  (assert-equal "42" (number->string 42))
  (assert-equal 42 (string->number "42"))
  (assert-equal "symbol" (symbol->string 'symbol))
  (assert-equal 'symbol (string->symbol "symbol")))

(define-test "core: texmacs environment and version"
  (assert-true (string? (texmacs-version)))
  (assert-false (string-null? (texmacs-version)))
  (assert-true (string? (get-env "OPERATING_SYSTEM"))))

