
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-widgets.scm
;; DESCRIPTION : manual debugging of LaTeX errors
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex-widgets)
  (:use (convert latex tmtex)))

(define (latex-error-digest err)
  (if (<= (tree-arity err) 1)
      "Miscellaneous error"
      (tree->string (tree-ref err 1))))

(tm-widget ((latex-errors-widget doc errs) quit)
  (let* ((digest (map latex-error-digest errs)))
    (padded
      (resize "600px" "200px"
        (scrollable
          (choice (ignore answer) digest (car digest))))
      ======
      (explicit-buttons
        (hlist
          >>>
          ("Ok" (quit)))))))

(tm-define (run-latex-buffer)
  (cond ((not (url-exists? (current-buffer)))
         (set-message "buffer must be on disk" "run-latex-buffer"))
        ((not (buffer-has-name? (current-buffer)))
         (set-message "buffer must have a name" "run-latex-buffer"))
        (else
          (let* ((opts (std-converter-options "texmacs-stree" "latex-document"))
                 (tm (current-buffer))
                 (nr (string-length (url-suffix tm)))
                 (tex (url-glue (url-unglue tm nr) "tex"))
                 (report (try-latex-export (buffer-get tm) opts tm tex)))
            (if (tree-atomic? report)
                (set-message (tree->string report) "run-latex-buffer")
                (let* ((doc (tree->string (tree-ref report 0)))
                       (errs (cdr (tree-children report))))
                  (if (null? errs)
                      (set-message "Generated LaTeX document contains no errors"
                                   "run-latex-buffer")
                      (dialogue-window (latex-errors-widget doc errs)
                                       noop "LaTeX errors"))))))))
