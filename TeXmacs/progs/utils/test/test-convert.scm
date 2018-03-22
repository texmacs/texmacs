
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-convert.scm
;; DESCRIPTION : running automatic test suites
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils test test-convert)
  (:use (texmacs texmacs tm-files)
        (doc tmdoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (strip-spaces s)
  (while (or (string-starts? s " ")
             (string-starts? s "\n")
             (string-starts? s "\t"))
    (set! s (string-drop s 1)))
  s)

(define (latex-file? s)
  (set! s (strip-spaces s))
  (or (string-starts? s "\\documentclass")
      (string-starts? s "\\documentstyle")
      (string-starts? s "%%%%%%%%%%%")))

(define (tar-file? s)
  (and (>= (string-length s) 256)
       (== (char->integer (string-ref s 99)) 0)
       (== (char->integer (string-ref s 255)) 0)))

(define (system-untar tar-file)
  (let* ((p (url-expand (url-append tar-file (url-parent))))
         (d (string-append "cd " (url->system p)))
         (u (string-append "tar -xvf " (url->system tar-file)))
         (c (string-append d "; " u)))         
    ;;(display* "-- " c "\n")
    (system c)))

(define (should-update? src-file dest-file)
  (or (not (url-exists? dest-file))
      (url-newer? src-file dest-file)))

(define (strip-suffix u)
  (with suffix (url-suffix u)
    (if (== suffix "") u
        (with r (url-unglue u (+ (string-length suffix) 1))
          (if (string? u) (url->string r) r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unpacking test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unpack-any-file x y src-dir dest-dir)
  ;;(display* "-- unpack-any-file " x ", " y ", " src-dir ", " dest-dir "\n")
  (let* ((src-file (url-expand (url-append src-dir x)))
         (dest-file (url-expand (url-append dest-dir y))))
    (when (should-update? src-file dest-file)
      (display* "TeXmacs]   Copying " (url->system y) "\n")
      (system-copy src-file dest-file))))

(define (unpack-texmacs-file x src-dir dest-dir)
  ;;(display* "-- unpack-texmacs-file " x ", " src-dir ", " dest-dir "\n")
  (unpack-any-file x x src-dir dest-dir))

(define (unpack-arxiv-file x src-dir dest-dir)
  ;;(display* "-- unpack-arxiv-file " x ", " src-dir ", " dest-dir "\n")
  (let* ((src-file (url-expand (url-append src-dir x)))
         (dest-sdir (url-expand (url-append dest-dir x)))
         (s (string-load src-file))
         (x-tex (string-append x ".tex"))
         (x-tar (string-append x ".tar")))
    (when (not (url-exists? dest-sdir))
      (display* "TeXmacs] Creating directory " (url->system dest-sdir) "\n")
      (system-mkdir (url-expand dest-sdir))
      (system-1 "chmod a+x" dest-sdir)
      (cond ((latex-file? s)
             (unpack-any-file x x-tex src-dir dest-sdir))
            ((tar-file? s)
             (unpack-any-file x x-tar src-dir dest-sdir)
             (display* "TeXmacs]   Unpacking " (url->system x-tar) "\n")
             (system-untar (url-append dest-sdir x-tar)))))))

(define (unpack-file x src-dir dest-dir type)
  ;;(display* "-- unpack-file " x ", " src-dir ", " dest-dir ", " type "\n")
  (cond ((== type "texmacs") (unpack-texmacs-file x src-dir dest-dir))
        ((== type "arxiv") (unpack-arxiv-file x src-dir dest-dir))))

(define (unpack-dir src-dir dest-dir type)
  ;;(display* "-- unpack-dir " src-dir ", " dest-dir ", " type "\n")
  (let* ((last (url->string (url-tail src-dir)))
         (accept (list "texmacs" "arxiv"))
         (ignore (list "CVS" ".svn" "prop-base" "text-base" ".DS_Store")))
    (set! type (or type (and (in? last accept) last)))
    (when (nin? last ignore)
      (when (url-exists? dest-dir)
        (display* "TeXmacs] Entering directory " (url->system dest-dir) "\n"))
      (when (not (url-exists? dest-dir))
        (display* "TeXmacs] Creating directory " (url->system dest-dir) "\n")
        (system-mkdir (url-expand dest-dir))
        (system-1 "chmod a+x" dest-dir))
      (let* ((u1 (url-append src-dir (url-wildcard "*")))
             (u2 (url->list (url-expand (url-complete u1 "dr"))))
             (u3 (map url->string (map url-tail u2)))
             (u4 (url->list (url-expand (url-complete u1 "fr"))))
             (u5 (map url->string (map url-tail u4))))
        (for-each (lambda (x) (unpack-dir (url-append src-dir x)
                                          (url-append dest-dir x) type)) u3)
        (for-each (lambda (x) (unpack-file x src-dir dest-dir type)) u5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building manuals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-manual* dir name lan next)
  ;;(display* "-- build-manual " dir ", " name ", " lan "\n")
  (let* ((root (cond ((== name "texmacs-user-manual")
                      (string-append "main/man-manual." lan ".tm"))
                     ((== name "texmacs-reference-manual")
                      (string-append "main/man-reference." lan ".tm"))
                     ((== name "texmacs-scheme-manual")
                      (string-append "devel/scheme/scheme." lan ".tm"))
                     (else "unknown.en.tm")))
         (doc-dir "$TEXMACS_DOC_PATH"))
    (if (url-exists? (url-unix doc-dir root))
        (let* ((old-lan (get-output-language))
               (new-lan (locale-to-language lan))
               (u (url-resolve (url-unix doc-dir root) "r"))
               (pdf (url-append dir (string-append name "." lan ".pdf")))
               (cont (lambda ()
                       (export-buffer-main (current-buffer) pdf "pdf" (list))
                       (set-output-language old-lan)
                       (user-delayed next))))
          (cond ((url-exists? pdf) (user-delayed next))
		((== new-lan old-lan) (tmdoc-expand-help-manual* u cont))
                (else
		  (set-output-language new-lan)
		  (delayed
		    (:idle 3000)
		    (tmdoc-expand-help-manual* u cont)))))
        (user-delayed next))))

(define (build-manuals-sub* dir l next)
  ;;(display* "-- build-manuals-sub " dir ", " l "\n")
  (if (null? l) (user-delayed next)
      (let* ((pdf (car l))
             (man (if (== (url-suffix pdf) "pdf") (strip-suffix pdf) pdf))
             (name (strip-suffix man))
             (lan (url-suffix man))
             (cont (lambda ()
                     (build-manuals-sub* dir (cdr l) next))))
        (build-manual* dir name lan cont))))

(define (build-manuals* orig-dir dest-dir next)
  ;;(display* "-- build-manuals " orig-dir ", " dest-dir "\n")
  (let* ((u1 (url-append orig-dir "manual"))
         (u2 (url-append dest-dir "manual"))
         (u3 (url-append u1 (url-wildcard "*")))
         (u4 (url->list (url-expand (url-complete u3 "fr"))))
         (u5 (map url->string (map url-tail u4))))
    (if (url-exists? u1)
        (begin
          (when (not (url-exists? u2))
            (display* "TeXmacs] Creating directory " u2 "\n")
            (system-mkdir (url-expand u2))
            (system-1 "chmod a+x" u2))
          (build-manuals-sub* u2 u5 next))
        (user-delayed next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test file conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-texmacs-file x dir)
  ;;(display* "-- test-texmacs-file " x ", " dir "\n")
  (when (string-ends? x ".tm")
    (let* ((root (string-drop-right x 3))
           (tm-file (url-append dir x))
           (pdf-file (url-append dir (string-append root ".pdf")))
           (tex-file (url-append dir (string-append root ".tex")))
           (opts (list :overwrite)))
      (when (should-update? tm-file pdf-file)
        (with-aux tm-file
          (display* "TeXmacs]   Exporting " x " to Pdf\n")
          (set! current-save-target pdf-file)
          (export-buffer-main (current-buffer) pdf-file "pdf" opts)
          (display* "TeXmacs]   Exporting " x " to LaTeX\n")
          (set! current-save-target tex-file)
          (export-buffer-main (current-buffer) tex-file "latex" opts))))))

(define (test-arxiv-file x dir)
  ;;(display* "-- test-arxiv-file " x ", " dir "\n")
  (when (string-ends? x ".tex")
    (let* ((u1 (url-append dir (url-wildcard "*.tex")))
           (u2 (url->list (url-expand (url-complete u1 "fr"))))
           (root (string-drop-right x 4))
           (tex-file (url-append dir x))
           (tm-file (url-append dir (string-append root ".tm")))
           (pdf-file (url-append dir (string-append root ".pdf")))
           (opts (list :overwrite)))
      (when (or (== (length u2) 1)
                (in? x (list "main.tex" "paper.tex" "article.tex")))
        (when (should-update? tex-file tm-file)
          (display* "TeXmacs]   Importing " x "\n")
          (convert-to-file tex-file "latex-file" "texmacs-file" tm-file)
          (with-aux tm-file
            (generate-all-aux)
            (update-current-buffer)
            (update-forced)
            (display* "TeXmacs]   Exporting " x " to Pdf\n")
            (set! current-save-target pdf-file)
            (export-buffer-main (current-buffer) pdf-file "pdf" opts)))))))

(define (test-file x dir type)
  ;;(display* "-- test-file " x ", " dir ", " type "\n")
  (cond ((== type "texmacs") (test-texmacs-file x dir))
        ((== type "arxiv") (test-arxiv-file x dir))))

(define (test-dir dir type)
  ;;(display* "-- test-dir " dir ", " type "\n")
  (let* ((last (url->string (url-tail dir)))
         (accept (list "texmacs" "arxiv")))
    (set! type (or type (and (in? last accept) last)))
    (when (url-exists? dir)
      (display* "TeXmacs] Entering directory " (url->system dir) "\n")
      (let* ((u1 (url-append dir (url-wildcard "*")))
             (u2 (url->list (url-expand (url-complete u1 "dr"))))
             (u3 (map url->string (map url-tail u2)))
             (u4 (url->list (url-expand (url-complete u1 "fr"))))
             (u5 (map url->string (map url-tail u4))))
        (for-each (lambda (x) (test-dir (url-append dir x) type)) u3)
        (for-each (lambda (x) (test-file x dir type)) u5)))))

(define (test-suite* orig-dir suffix next)
  (let* ((dir (url-expand (url-complete orig-dir "dr")))
         (head (url-head dir))
         (tail (url->string (url-tail dir)))
         (dest-dir (url-append head (string-append tail suffix))))
    (unpack-dir dir dest-dir #f)
    (test-dir dest-dir #f)
    (build-manuals* dir dest-dir next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking the test suite against a reference build
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define missing-dirs (list))
(define missing-files (list))
(define changed-files (list))
(define changed-sizes (list))
(define changed-properties (list))

(define (compare-text-file x dir ref)
  ;;(display* "-- compare-text-file " x ", " dir ", " ref "\n")
  (let* ((new-file (url-append dir x))
         (ref-file (url-append ref x)))
    (when (and (not (url-exists? new-file)) (url-exists? ref-file))
      (display* "TeXmacs] Missing file " (url->system x) "\n")
      (set! missing-files (cons new-file missing-files)))
    (when (and (url-exists? dir) (url-exists? ref))
      (display* "TeXmacs]   Comparing " (url->system x) "\n")
      (let* ((new-s (string-load new-file))
             (ref-s (string-load ref-file))
             (dist (string-distance new-s ref-s)))
        (cond ((== dist 0) (noop))
              ((< dist 5) (noop))
              (else
                (display* "TeXmacs]     Changed " dist " bytes\n")
                (with c (list new-file dist)
                  (set! changed-files (cons c changed-files)))))))))

(define (pdf-keep? s)
  (and (not (string-starts? s "CreationDate:"))
       (not (string-starts? s "File size:"))))

(define (pdf-info u)
  (if (or (not (url-exists? u))
          (not (url-exists-in-path? "pdfinfo")))
      ""
      (let* ((cmd (string-append "pdfinfo " (url->system u)))
             (info (eval-system cmd))
             (l (cpp-string-tokenize info "\n"))
             (k (list-filter l pdf-keep?)))
        (cpp-string-recompose k "\n"))))

(define (compare-pdf-file x dir ref)
  ;;(display* "-- compare-pdf-file " x ", " dir ", " ref "\n")
  (let* ((new-file (url-append dir x))
         (ref-file (url-append ref x)))
    (when (and (not (url-exists? new-file)) (url-exists? ref-file))
      (display* "TeXmacs] Missing file " (url->system x) "\n")
      (set! missing-files (cons new-file missing-files)))
    (when (and (url-exists? dir) (url-exists? ref))
      (display* "TeXmacs]   Comparing " (url->system x) "\n")
      (let* ((new-size (string-length (string-load new-file)))
             (ref-size (string-length (string-load ref-file)))
             (delta-size (- new-size ref-size))
             (new-s (pdf-info new-file))
             (ref-s (pdf-info ref-file)))
        (when (> (abs delta-size) 150)
          (display* "TeXmacs]     Size change of " delta-size " bytes\n")
          (with c (list new-file delta-size)
            (set! changed-sizes (cons c changed-sizes))))
        (when (!= new-s ref-s)
          (display* "TeXmacs]     Changed Pdf properties\n")
          (with c (list new-file new-s)
            (set! changed-properties (cons c changed-properties))))))))

(define (compare-texmacs-file x dir ref)
  ;;(display* "-- compare-texmacs-file " x ", " dir ", " ref "\n")
  (cond ((string-ends? x ".pdf")
         (compare-pdf-file x dir ref))
        ((string-ends? x ".tex")
         (compare-text-file x dir ref))))

(define (compare-arxiv-file x dir ref)
  ;;(display* "-- compare-arxiv-file " x ", " dir ", " ref "\n")
  (cond ((string-ends? x ".tm")
         (compare-text-file x dir ref))
        ((string-ends? x ".pdf")
         (compare-pdf-file x dir ref))))

(define (compare-manual-file x dir ref)
  ;;(display* "-- compare-manual-file " x ", " dir ", " ref "\n")
  (cond ((string-ends? x ".pdf")
         (compare-pdf-file x dir ref))))

(define (compare-file x dir ref type)
  ;;(display* "-- compare-file " x ", " dir ", " ref ", " type "\n")
  (cond ((== type "texmacs") (compare-texmacs-file x dir ref))
        ((== type "arxiv") (compare-arxiv-file x dir ref))
        ((== type "manual") (compare-manual-file x dir ref))))

(define (compare-dir dir ref type)
  ;;(display* "-- compare-dir " dir ", " ref ", " type "\n")
  (let* ((last (url->string (url-tail dir)))
         (accept (list "texmacs" "arxiv" "manual")))
    (set! type (or type (and (in? last accept) last)))
    (when (and (not (url-exists? dir)) (url-exists? ref))
      (display* "TeXmacs] Missing directory " (url->system dir) "\n")
      (set! missing-dirs (cons dir missing-dirs)))
    (when (and (url-exists? dir) (url-exists? ref))
      (display* "TeXmacs] Entering directory " (url->system dir) "\n")
      (let* ((u1 (url-append ref (url-wildcard "*")))
             (u2 (url->list (url-expand (url-complete u1 "dr"))))
             (u3 (map url->string (map url-tail u2)))
             (u4 (url->list (url-expand (url-complete u1 "fr"))))
             (u5 (map url->string (map url-tail u4))))
        (for-each (lambda (x) (compare-dir (url-append dir x)
                                           (url-append ref x) type)) u3)
        (for-each (lambda (x) (compare-file x dir ref type)) u5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate status report
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (status-entry title dir x)
  (let* ((u (if (list? x) (car x) x))
         (d (url-delta (url-append dir "dummy") u)))
    (display* "  " (url->system d) "\n")
    `(concat (item) (verbatim ,(url->system d)))))

(define (status-section title dir l)
  (if (null? l) l
      (begin
        (display* "\n--------------------------------------------------\n")
        (display* title)
        (display* "\n--------------------------------------------------\n")
        (list `(section* ,title)
              `(itemize
                (document
                  ,@(map (cut status-entry title dir <>) (reverse l))))))))

(define (status-report dir)
  (let* ((u (url-append dir "status-report.tm"))
         (l1 (status-section "Missing directories"
                             dir missing-dirs))
         (l2 (status-section "Missing files"
                             dir missing-files))
         (l3 (status-section "Files with important changes"
                             dir changed-files))
         (l4 (status-section "Files with changed sizes"
                             dir changed-sizes))
         (l5 (status-section "Pdf files with changed properties"
                             dir changed-properties))
         (l (append l1 l2 l3 l4 l5)))
    (if (null? l)
        (if (url-exists? u) (system-remove u))
        (let* ((body `(document ,@l))
               (doc `(document
                       (TeXmacs ,(texmacs-version))
                       (style "generic")
                       (body ,body)))
               (t (stree->tree doc))
               (d (convert t "texmacs-tree" "texmacs-document")))
          (string-save d u)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (delayed-quit)
  (user-delayed quit-TeXmacs))

(define (continue opts)
  (when (nnull? opts)
    (for-each (lambda (opt) (opt)) opts)))

(tm-define (build-manual man-file . opts)
  (when (not (url-rooted? man-file))
    (with pwd (getenv "PWD")
      (set! man-file (url-append pwd man-file))))
  (let* ((dir (url-head man-file))
         (pdf (url->system (url-tail man-file)))
         (man (if (== (url-suffix pdf) "pdf") (strip-suffix pdf) pdf))
         (name (strip-suffix man))
         (lan (url-suffix man))
         (cur (current-buffer))
         (back (lambda () (switch-to-buffer cur) (continue opts))))
    (build-manual* dir name lan back)))

(tm-define (build-ref-suite orig-dir . opts)
  (let* ((cur (current-buffer))
         (back (lambda () (switch-to-buffer cur) (continue opts))))
    (test-suite* orig-dir "-ref" back)))

(define (run-test-suite-next orig-dir)
  (let* ((dir (url-expand (url-complete orig-dir "dr")))
         (head (url-head dir))
         (tail (url->string (url-tail dir)))
         (ref-dir (url-append head (string-append tail "-ref")))
         (check-dir (url-append head (string-append tail "-check"))))
    (when (url-exists? ref-dir)
      (set! missing-dirs (list))
      (set! missing-files (list))
      (set! changed-files (list))
      (set! changed-sizes (list))
      (set! changed-properties (list))
      (compare-dir check-dir ref-dir #f)
      (status-report check-dir))))

(tm-define (run-test-suite orig-dir . opts)
  (let* ((cur (current-buffer))
         (next (lambda () (run-test-suite-next orig-dir) (continue opts)))
         (cont (lambda () (switch-to-buffer cur) (user-delayed next))))
    (test-suite* orig-dir "-check" cont)))
