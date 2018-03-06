
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
  (:use (texmacs texmacs tm-files)))

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
         (ignore (list "CVS" ".svn" "prop-base" "text-base")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-test-suite orig-dir)
  (let* ((dir (url-expand (url-complete orig-dir "dr")))
         (head (url-head dir))
         (tail (url->string (url-tail dir)))
         (ref-dir (url-append head (string-append tail "-ref")))
         ;;(check-dir (url-append head (string-append tail "-check")))
         ;;(dest-dir (if (url-exists? ref-dir) check-dir ref-dir))
         (dest-dir ref-dir)
         )
    (unpack-dir dir dest-dir #f)
    (test-dir dest-dir #f)))
