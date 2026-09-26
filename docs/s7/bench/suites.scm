;; Times the 15 regression suites that run on both interpreters.
;; Usage: texmacs.bin -x '(load "docs/s7/bench/suites.scm")'
;; Output: one "=== suite -> result" line per suite, then SUITES-TIME <ms>.
(define (bench-try name thunk)
  (catch #t (lambda () (display* "=== " name " -> " (thunk) "\n"))
    (lambda (k . r) (display* "=== " name " FAILED: " k " " r "\n"))))
(lazy-define-force run-all-tests)
(define suites-t0 (texmacs-time))
(for-each (lambda (s) (bench-try s (lambda () ((eval (string->symbol s))))))
  '("regtest-abbrevs" "regtest-logic" "regtest-tm-glue" "regtest-tm-convert"
    "regtest-htmltm" "regtest-xmltm" "regtest-tmlength" "regtest-environment"
    "regtest-mathtm" "regtest-tmhtml" "regtest-tmmltm" "regtest-prog-format"
    "regtest-tm-define" "regtest-tm-dialogue" "regtest-cite-sort"))
(display* "SUITES-TIME " (- (texmacs-time) suites-t0) "\n")
(quit-TeXmacs)
