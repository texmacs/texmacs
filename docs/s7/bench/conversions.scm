;; Conversions of four documents, each task once cold and three times warm.
;; Usage: texmacs.bin -x '(load "docs/s7/bench/conversions.scm")'
;; Output: BENCH <task> cold <ms> warm <median ms>. The exported files go
;; to $TEXMACS_HOME_PATH/system/tmp/s7-bench.
(system-mkdir (url-unix "$TEXMACS_HOME_PATH" "system/tmp/s7-bench"))
(define (bench-out name ext)
  (url-unix "$TEXMACS_HOME_PATH"
            (string-append "system/tmp/s7-bench/" name "." ext)))
(define bench-docs
  (map (lambda (f) (url-unix "$TEXMACS_PATH" f))
       '("doc/about/changes/change-log.en.tm"
         "doc/devel/format/environment/env-page.en.tm"
         "examples/texts/bigtable-test.tm"
         "examples/texts/superscript-test-bis.tm")))
(define (median3 l) (cadr (sort l <)))
(define (bench name thunk)
  (let* ((t0 (texmacs-time))
         (dummy (thunk))
         (cold (- (texmacs-time) t0))
         (warm (map (lambda (i)
                      (let ((t1 (texmacs-time))) (thunk) (- (texmacs-time) t1)))
                    '(1 2 3))))
    (display* "BENCH " name " cold " cold " warm " (median3 warm) "\n")))
(define (doc-name u) (url->string (url-basename u)))
(define docs-trees #f)
(define (load-docs)
  (set! docs-trees (map (lambda (u) (tree-import u "texmacs")) bench-docs)))
(define (export-all fm ext)
  (for-each (lambda (u)
              (with-aux u
                (export-buffer-main (current-buffer) (bench-out (doc-name u) ext)
                                    fm (list :overwrite))))
            bench-docs))
(define (import-all fm ext)
  (for-each (lambda (u) (tree-import (bench-out (doc-name u) ext) fm)) bench-docs))
(define (menus n)
  (do ((i 0 (+ i 1))) ((= i n))
    (menu-expand '(horizontal (link texmacs-main-icons)))
    (menu-expand '(horizontal (link texmacs-mode-icons)))
    (menu-expand '(vertical (link texmacs-menu)))))
(define (stree-roundtrip)
  (for-each (lambda (t) (stree->tree (tree->stree t))) docs-trees))
(bench "load-4-docs" load-docs)
(bench "tree->stree->tree" stree-roundtrip)
(bench "export-latex" (lambda () (export-all "latex" "tex")))
(bench "export-html" (lambda () (export-all "html" "html")))
(bench "import-latex" (lambda () (import-all "latex" "tex")))
(bench "import-html" (lambda () (import-all "html" "html")))
(bench "menu-expand-x10" (lambda () (menus 10)))
(quit-TeXmacs)
