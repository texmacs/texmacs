;; Microbenchmarks of the C++ <-> Scheme boundary: strings in both
;; directions, the per-call cost of glue, and tree <-> stree conversions.
;; Usage: texmacs.bin -x '(load "docs/s7/bench/marshal.scm")'
;; Output: MARSHAL lines.
(define (time-it n thunk)
  (let ((t0 (texmacs-time)))
    (do ((i 0 (+ i 1))) ((= i n)) (thunk))
    (- (texmacs-time) t0)))
(define (report name n ms . extra)
  (display* "MARSHAL " name ": " n " calls, " ms " ms, "
            (if (> n 0) (/ (* 1000000.0 ms) n) 0) " ns/call" (apply string-append extra) "\n"))
(define (make-str k) (make-string k #\a))
(define sizes '(10 1000 100000))
;; string -> C++ (string->tree: tmscm_to_string, then an atomic tree)
(for-each (lambda (k)
            (let* ((s (make-str k)) (n (max 20 (quotient 20000000 (+ k 100)))))
              (report (string-append "string->tree " (number->string k) " B") n
                      (time-it n (lambda () (string->tree s))))))
          sizes)
;; C++ -> string (tree->string: string_to_tmscm, c_string + s7 copy)
(for-each (lambda (k)
            (let* ((t (string->tree (make-str k))) (n (max 20 (quotient 20000000 (+ k 100)))))
              (report (string-append "tree->string " (number->string k) " B") n
                      (time-it n (lambda () (tree->string t))))))
          sizes)
;; a glue call with a tiny string in and a bool out, for the per-call cost
(report "string-alpha? \"a\"" 1000000 (time-it 1000000 (lambda () (string-alpha? "a"))))
;; a Scheme primitive, for comparison
(report "string-length \"a\" (Scheme primitive)" 1000000 (time-it 1000000 (lambda () (string-length "a"))))
;; trees <-> strees on a real document
(define doc (tree-import (url-unix "$TEXMACS_PATH" "doc/about/changes/change-log.en.tm") "texmacs"))
(define st (tree->stree doc))
(define (count-nodes x) (if (pair? x) (apply + 1 (map count-nodes (cdr x))) 1))
(define (count-bytes x) (cond ((string? x) (string-length x)) ((pair? x) (apply + (map count-bytes (cdr x)))) (else 0)))
(display* "MARSHAL change-log: " (count-nodes st) " nodes, " (count-bytes st) " bytes of text\n")
(report "tree->stree change-log" 50 (time-it 50 (lambda () (tree->stree doc))))
(report "stree->tree change-log" 50 (time-it 50 (lambda () (stree->tree st))))
(quit-TeXmacs)
