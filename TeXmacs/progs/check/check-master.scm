
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : check-master.scm
;; DESCRIPTION : regression tests
;; COPYRIGHT   : (C) 2014  Joris van der Hoeven
;;                   2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (check check-master)
  (:use (convert html htmltm-test)
        (convert tools xmltm-test)
        (convert tmml tmmltm-test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test LaTeX export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-pdflatex tex-file)
  (and (url-exists? tex-file)
       (let* ((tex-dir  (url-head tex-file))
              (pdf-file (url-glue (url-unglue tex-file 4) ".pdf"))
              (log-file (url-glue (url-unglue tex-file 4) ".log"))
              (cmd1 (string-append "cd " (system-url->string tex-dir)))
              (cmd2 (string-append "pdflatex -interaction=batchmode "
                                   (url->string (url-tail tex-file))))
              (cmd  (string-append cmd1 "; " cmd2 " > /dev/null")))
         (system-remove pdf-file)
         (system-remove log-file)
         (system cmd)
         (and (url-exists? log-file)
              (list (url-exists? pdf-file)
                    (number-latex-errors log-file)
                    (number-latex-pages log-file))))))

(define (check-latex-export-one tm-file)
  (display* "Checking LaTeX export of " (url->string tm-file) "...\n")
  (with latex-file (url-glue (url-unglue tm-file 3) ".tex")
    (with-aux tm-file
      (system-remove latex-file)
      (if (url? latex-file) (set! current-save-target latex-file))
      (export-buffer-main (current-buffer) latex-file
                          "latex" (list :overwrite))
      (display* "Checking LaTeX on " (url->string latex-file) "...\n")
      (with status (run-pdflatex latex-file)
        (cond ((not status) (display* "  LaTeX export failed\n"))
              ((not (car status)) (display* "  PdfLaTeX failed\n"))
              (else
                (with (ok? errs pages) status
                  (when (or (!= errs 0) (<= pages 0))
                    (display* "  PdfLaTeX encountered " errs " error(s)\n")
                    (display* "  PdfLaTeX produced " pages " page(s)\n")))))))))

(tm-define (check-latex-export u)
  (:synopsis "Try to export and LaTeX all TeXmacs files inside @u")
  (let* ((tm-files (url-append u (url-append (url-any) "*.tm")))
         (l (url->list (url-expand (url-complete tm-files "fr")))))
    (for (x l)
      (check-latex-export-one x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All regression tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (check-all u)
  (:synopsis "Run all regression tests in directory @u.")
  (check-latex-export u))

(tm-define (run-checks)
  (check-latex-export "$TEXMACS_CHECKS/latex-export"))

(tm-define (run-all-tests)
  (regtest-htmltm)
  (regtest-xmltm)
  ;(regtest-tmmltm)
)
