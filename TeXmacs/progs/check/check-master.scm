
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : check-master.scm
;; DESCRIPTION : regression tests
;; COPYRIGHT   : (C) 2014  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (check check-master))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test LaTeX export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-latex-export-one tm-file)
  (display* "Checking LaTeX export of " tm-file "...\n")
  (with latex-file (url-glue (url-unglue tm-file 3) ".tex")
    (with-aux tm-file
      (if (url? latex-file) (set! current-save-target latex-file))
      (export-buffer-main (current-buffer) latex-file
                          "latex" (list :overwrite)))))

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
