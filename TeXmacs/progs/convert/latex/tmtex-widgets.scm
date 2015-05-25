
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
  (tree->string (tree-ref err 1)))

(define (string->document s)
  (with l (string-tokenize-by-char (string->tmstring s) #\newline)
    `(document ,@l)))

(define (latex-error-doc* err)
  (if (<= (tree-arity err) 2)
      (string->document (tree->string (tree-ref err 0)))
      `(document
         (padded
           (with "color" "dark red"
             ,(string->document (tree->string (tree-ref err 2))))
           "0fn" "0.5fn")
         (padded
           (with "color" "black"
             ,(string->document (tree->string (tree-ref err 3))))
           "0fn" "0.5fn")
         (padded
           (with "color" "dark blue"
             ,(string->document (tree->string (tree-ref err 4))))
           "0fn" "0.5fn")
         (padded
           (with "color" "black"
             ,(string->document (tree->string (tree-ref err 5))))
           "0fn" "0.5fn"))))

(define (latex-error-doc err)
  `(document (code ,(latex-error-doc* err))))

(tm-widget ((latex-errors-widget doc errs) quit)
  (let* ((digest (map latex-error-digest errs))
         (errnr 0)
         (err (list-ref errs errnr))
         (sel (lambda (msg)
                (set! errnr (or (list-find-index digest (cut == <> msg)) 0))
                (set! err (list-ref errs errnr))
                (buffer-set-body "tmfs://aux/latex-error"
                                 (latex-error-doc (list-ref errs errnr))))))
    (padded
      (resize "800px" "200px"
        (scrollable
          (choice (sel answer) digest (latex-error-digest err))))
      ======
      (resize "800px" "200px"
        (texmacs-input (latex-error-doc (list-ref errs errnr))
                       `(style (tuple "generic"))
                       (string-append "tmfs://aux/latex-error"))))))

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
