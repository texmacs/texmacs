
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-latex-export.scm
;; DESCRIPTION : running automatic LaTeX export tests
;; COPYRIGHT   : (C) 2021  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils test test-latex-export)
  (:use (texmacs texmacs tm-files)
        (generic document-style)
        (utils library cursor)
        (check check-master)
        (doc tmdoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting to LaTeX using different LaTeX styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-latex-export-file dir file style)
  (let* ((root (string-drop-right file 3))
         (style-dir (url-append dir style))
         (tm-file (url-append (url-append dir "source") file))
         (tex-file (url-append style-dir (string-append root ".tex")))
         (opts (list :overwrite)))
    (buffer-load tm-file)
    (delayed
      (:idle 1000)
      (switch-to-buffer tm-file)
      (init-default "page-type")
      (init-default "font")
      (init-default "font-base-size")
      (remove-style-package "stix-font")
      (remove-style-package "bonum-font")
      (remove-style-package "pagella-font")
      (remove-style-package "schola-font")
      (remove-style-package "termes-font")
      (set-main-style style)
      (system-mkdir style-dir)
      (set! current-save-target tex-file)
      (display* "Export " file ", style = " style "\n")
      (export-buffer-main (current-buffer) tex-file "latex" opts)
      (with status (run-pdflatex tex-file)
        (cond ((not status) (display* "  LaTeX export failed\n"))
              ((not (car status)) (display* "  PdfLaTeX failed\n"))
              (else
                (with (ok? errs pages) status
                  (when (or (!= errs 0) (<= pages 0))
                    (display* "  PdfLaTeX encountered " errs " error(s)\n")
                    (display* "  PdfLaTeX produced " pages " page(s)\n")
                    ))))))))

(tm-define (run-latex-export-suite orig-dir)
  (let* ((old (current-buffer))
         (dir (url-expand (url-complete orig-dir "dr")))
         (sub-dir (url-append orig-dir "source"))
         (sdir (url-expand (url-complete sub-dir "dr")))
         (u1 (url-append sdir (url-wildcard "*.tm")))
         (files (url->list (url-expand (url-complete u1 "fr"))))
         (styles (list "article" "acmart"
                       "acmtog" "aip" "amsart" "aps"
                       "elsarticle" "ieeeconf" "ieeetran" "ifac"
                       "llncs" "sigconf" "svjour")))
    (for (file files)
      (for (style styles)
        (with name (url->string (url-tail file))
          (run-latex-export-file orig-dir name style))))
    (delayed
      (:idle 1000)
      (switch-to-buffer old))))
