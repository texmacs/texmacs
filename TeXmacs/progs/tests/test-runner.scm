;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-runner.scm
;; DESCRIPTION : Unit test runner and assertion framework for TeXmacs
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-runner))

;; Test counters and state
(define-public *test-total* 0)
(define-public *test-passed* 0)
(define-public *test-failed* 0)
(define-public *test-errors* 0)
(define-public *registered-tests* '())
(define-public *current-test-name* "")
(define-public *current-test-failed?* #f)

;; Register a test case
(define-public-macro (define-test name . body)
  `(set! *registered-tests*
         (append *registered-tests*
                 (list (cons ,name (lambda () ,@body))))))

;; Assertions
(define-public (assert-true val . msg)
  (set! *test-total* (+ *test-total* 1))
  (if val
      (set! *test-passed* (+ *test-passed* 1))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (set! *current-test-failed?* #t)
        (display "    [ASSERT FAILED] expected #t, got: ")
        (display (object->string val))
        (when (pair? msg) (display (string-append " (" (car msg) ")")))
        (newline))))

(define-public (assert-false val . msg)
  (set! *test-total* (+ *test-total* 1))
  (if (not val)
      (set! *test-passed* (+ *test-passed* 1))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (set! *current-test-failed?* #t)
        (display "    [ASSERT FAILED] expected #f, got: ")
        (display (object->string val))
        (when (pair? msg) (display (string-append " (" (car msg) ")")))
        (newline))))

(define-public (assert-equal expected actual . msg)
  (set! *test-total* (+ *test-total* 1))
  (if (equal? expected actual)
      (set! *test-passed* (+ *test-passed* 1))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (set! *current-test-failed?* #t)
        (display "    [ASSERT FAILED] ")
        (when (pair? msg) (display (string-append (car msg) ": ")))
        (newline)
        (display "      Expected: ") (display (object->string expected)) (newline)
        (display "      Actual:   ") (display (object->string actual)) (newline))))

(define-public (assert-not-equal unexpected actual . msg)
  (set! *test-total* (+ *test-total* 1))
  (if (not (equal? unexpected actual))
      (set! *test-passed* (+ *test-passed* 1))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (set! *current-test-failed?* #t)
        (display "    [ASSERT FAILED] expected NOT equal to: ")
        (display (object->string unexpected))
        (when (pair? msg) (display (string-append " (" (car msg) ")")))
        (newline))))

(define-public (assert-null? val . msg)
  (assert-true (null? val) (if (pair? msg) (car msg) "expected null")))

(define-public (assert-not-null? val . msg)
  (assert-true (not (null? val)) (if (pair? msg) (car msg) "expected non-null")))

(define-public-macro (assert-error expr . msg)
  `(let ((threw? #f))
     (catch #t
       (lambda () ,expr)
       (lambda (key . args) (set! threw? #t)))
     (assert-true threw? (if (pair? (list ,@msg)) (car (list ,@msg)) "expected error"))))

;; Main test runner entry point
(tm-define (run-unit-tests)
  (display "\n=======================================================\n")
  (display "  Running TeXmacs Unit Test Suite\n")
  (display "=======================================================\n\n")

  (set! *test-total* 0)
  (set! *test-passed* 0)
  (set! *test-failed* 0)
  (set! *test-errors* 0)
  (set! *registered-tests* '())

  ;; Load test suites dynamically into current module
  (eval '(use-modules (tests test-core)
                      (tests test-url)
                      (tests test-tree)
                      (tests test-preferences)
                      (tests test-regression))
        (current-module))

  (let ((start-time (texmacs-time)))
    (for-each
      (lambda (test-pair)
        (let ((name (car test-pair))
              (func (cdr test-pair)))
          (set! *current-test-name* name)
          (set! *current-test-failed?* #f)
          (catch #t
            (lambda ()
              (func)
              (if (not *current-test-failed?*)
                  (display (string-append "  [PASS] " name "\n"))
                  (display (string-append "  [FAIL] " name "\n"))))
            (lambda (key . args)
              (set! *test-errors* (+ *test-errors* 1))
              (display (string-append "  [ERROR] " name " (exception: "
                                      (object->string key) " "
                                      (object->string args) ")\n"))))))
      *registered-tests*)

    (let* ((end-time (texmacs-time))
           (duration (/ (- end-time start-time) 1000.0)))
      (display "\n=======================================================\n")
      (display "  Test Results Summary\n")
      (display "=======================================================\n")
      (display (string-append "  Total assertions: " (number->string *test-total*) "\n"))
      (display (string-append "  Passed:           " (number->string *test-passed*) "\n"))
      (display (string-append "  Failed:           " (number->string *test-failed*) "\n"))
      (display (string-append "  Errors:           " (number->string *test-errors*) "\n"))
      (display (string-append "  Duration:         " (number->string duration) "s\n"))
      (display "=======================================================\n")
      (if (and (= *test-failed* 0) (= *test-errors* 0))
          (begin
            (display "  OVERALL: ALL TESTS PASSED\n\n")
            (quit-TeXmacs-code 0))
          (begin
            (display "  OVERALL: SOME TESTS FAILED\n\n")
            (quit-TeXmacs-code 1))))))
