;; Regenerates the full manual (124 pages), as benchmark-manual does,
;; timing each phase. Usage: texmacs.bin -x '(load "docs/s7/bench/manual.scm")'
;; Output: MANUAL <phase> <ms> lines, then the page count.
(define manual-t0 (texmacs-time))
(define manual-last manual-t0)
(define (manual-phase name)
  (let ((now (texmacs-time)))
    (display* "MANUAL " name " " (- now manual-last) " ms (total " (- now manual-t0) " ms)\n")
    (set! manual-last now)))
(exec-delayed
  (lambda ()
    (let ((root (url-resolve (url-unix "$TEXMACS_DOC_PATH" "main/man-manual.en.tm") "r"))
          (update (lambda (name cont)
                    (generate-all-aux)
                    (update-current-buffer)
                    (manual-phase name)
                    (exec-delayed cont))))
      (tmdoc-expand-help root "book")
      (manual-phase "expand")
      (exec-delayed
        (lambda ()
          (update "update-1"
            (lambda ()
              (update "update-2"
                (lambda ()
                  (update "update-3"
                    (lambda ()
                      (display* "MANUAL pages " (get-page-count) "\n")
                      (buffer-pretend-saved (current-buffer))
                      (quit-TeXmacs))))))))))))
