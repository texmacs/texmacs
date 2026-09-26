;; Runs the regression suites for CI and reports the outcome with a marker
;; line, since the exit status of TeXmacs does not reflect it.
;; Usage: texmacs.bin -x '(load ".github/scripts/run-tests.scm")'

(define (ci-finish ok?)
  (display (if ok? "CI-TESTS-OK\n" "CI-TESTS-FAILED\n"))
  (quit-TeXmacs))

(catch #t
  (lambda ()
    (lazy-define-force run-all-tests)
    (run-all-tests)
    (ci-finish #t))
  (lambda (key . args)
    (display* "CI error: " key " " args "\n")
    (ci-finish #f)))
