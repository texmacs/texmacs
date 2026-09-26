;; Exports the change log to LaTeX once, then 8 more times (warm).
;; Usage: texmacs.bin -x '(load "docs/s7/bench/latex-loop.scm")'
;; Output: LOOP-DONE <ms> for the 8 warm exports. The .tex file goes to
;; $TEXMACS_HOME_PATH/system/tmp/s7-bench.
(system-mkdir (url-unix "$TEXMACS_HOME_PATH" "system/tmp/s7-bench"))
(define u (url-unix "$TEXMACS_PATH" "doc/about/changes/change-log.en.tm"))
(define (once)
  ;; through an auxiliary buffer, as check-latex-export does
  (with-aux u
    (export-buffer-main (current-buffer)
                        (url-unix "$TEXMACS_HOME_PATH"
                                  "system/tmp/s7-bench/change-log.tex")
                        "latex" (list :overwrite))))
(once)
(define t0 (texmacs-time))
(do ((i 0 (+ i 1))) ((= i 8)) (once))
(display* "LOOP-DONE " (- (texmacs-time) t0) " ms\n")
(quit-TeXmacs)
