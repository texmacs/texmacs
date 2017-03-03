
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
  (:use (convert latex tmtex)
        (utils library cursor)
        (check check-master)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The widget for examing LaTeX errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-error-buffer)
  (string->url "tmfs://aux/latex-error"))

(define (latex-source-buffer)
  (string->url "tmfs://aux/latex-source"))

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

(define (decode-path t)
  (and (tree-func? t 'tuple)
       (list-and (map tree-integer? (tree-children t)))
       (map tree->number (tree-children t))))

(define (latex-error-track buf err)
  (when (>= (tree-arity err) 8)
    (let* ((p (decode-path (tree-ref err 7)))
           (b (buffer-get-body buf))
           (src (apply tree-ref (cons b p))))
      (when src
        (with-buffer buf
          (tree-select src)
          (tree-go-to src :start))))))

(define (latex-error-show doc err)
  (when (>= (tree-arity err) 7)
    (let* ((pos (tree->number (tree-ref err 6)))
           (l (- (get-line-number doc pos) 1))
           (c (get-column-number doc pos))
           (src (buffer-get-body "tmfs://aux/latex-source")))
      (and-with line (tree-ref src l)
        (when (and (tree-atomic? line)
                   (<= c (string-length (tree->string line))))
          (with-buffer "tmfs://aux/latex-source"
            (let* ((p (tree->path line))
                   (b (append p (list 0)))
                   (e (append p (list c))))
              (selection-set b e)
              (tree-go-to line c))))))))

(tm-widget ((latex-errors-widget buf doc errs) quit)
  (let* ((digest (map latex-error-digest errs))
         (errnr 0)
         (err (list-ref errs errnr))
         (sel (lambda (msg)
                (set! errnr (or (list-find-index digest (cut == <> msg)) 0))
                (set! err (list-ref errs errnr))
                (buffer-set-body "tmfs://aux/latex-error"
                                 (latex-error-doc (list-ref errs errnr)))
                (latex-error-track buf err)
                (latex-error-show doc err))))
    (padded
      (resize "800px" "200px"
        (scrollable
          (choice (sel answer) digest "")))
      ======
      (resize "800px" "150px"
        (texmacs-input (latex-error-doc (list-ref errs errnr))
                       `(style (tuple "generic"))
                       (latex-error-buffer)))
      ======
      (resize "800px" "450px"
        (texmacs-input (string->document doc)
                       `(style (tuple "verbatim-source"))
                       (latex-source-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert, run pdflatex, and examine errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-buffer-suffixed suf)
  (and (url-exists? (current-buffer))
       (buffer-has-name? (current-buffer))
       (let* ((tm (current-buffer))
              (nr (string-length (url-suffix tm)))
              (tex (url-glue (url-unglue tm nr) suf)))
         (and (== (url-suffix tm) "tm") tex))))

(define (latex-export)
  (with tex (current-buffer-suffixed "tex")
    (if (not tex)
        (set-message "TeXmacs buffer on disk expected" "latex-export")
        (export-buffer tex))))

(define (latex-run)
  (cond ((not (url-exists? (current-buffer)))
         (set-message "buffer must be on disk" "latex-run"))
        ((not (buffer-has-name? (current-buffer)))
         (set-message "buffer must have a name" "latex-run"))
        ((not (current-buffer-suffixed "tex"))
         (set-message "TeXmacs buffer expected" "latex-run"))
        (else
          (let* ((opts (std-converter-options "texmacs-stree" "latex-document"))
                 (tm (current-buffer))
                 (tex (current-buffer-suffixed "tex"))
                 (report (with-global current-save-target tex
                           (try-latex-export (buffer-get tm) opts tm tex))))
            (if (tree-atomic? report)
                (set-message (tree->string report) "latex-run")
                (let* ((buf (current-buffer))
                       (doc (tree->string (tree-ref report 0)))
                       (errs (cdr (tree-children report))))
                  (if (null? errs)
                      (set-message "Generated LaTeX document contains no errors"
                                   "latex-run")
                      (dialogue-window (latex-errors-widget buf doc errs)
                                       noop "LaTeX errors"
                                       (latex-error-buffer)
                                       (latex-source-buffer)))))))))

(define (latex-preview)
  (let* ((tex (current-buffer-suffixed "tex"))
         (pdf (current-buffer-suffixed "pdf")))
    (if (not (and tex pdf))
        (set-message "TeXmacs buffer on disk expected" "latex-export")
        (begin
          (export-buffer tex)
          (run-pdflatex tex)
          (preview-file pdf)))))

(menu-bind tmtex-menu
  ("Export" (latex-export))
  ("Run" (latex-run))
  ("Preview" (latex-preview)))
