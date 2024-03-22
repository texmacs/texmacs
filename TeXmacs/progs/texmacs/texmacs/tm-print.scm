
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-print.scm
;; DESCRIPTION : routines for printing documents
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-print)
  (:use (ice-9 rdelim)
        (texmacs texmacs tm-files)
        (dynamic fold-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to obtain the papersize in this order from
;; - the environment variable PAPERSIZE
;; - the contents of the file specified by the PAPERCONF environment variable
;; - the contents of the file "/etc/papersize"
;; or else default to "a4"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define supported-sizes
  '("a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9"
    "b0" "b1" "b2" "b3" "b4" "b5" "b6" "b7" "b8" "b9"
    "archA" "archB" "archC" "archD" "archE"
    "10x14" "11x17" "C5" "Comm10" "DL" "executive" "halfletter"
    "halfexecutive" "ledger" "legal" "letter" "Monarch"
    "csheet" "dsheet" "flsa" "flse" "folio"
    "lecture note" "note" "quarto" "statement" "tabloid"
    "16:9" "8:5" "3:2" "4:3" "5:4"
    "user"))

(define standard-sizes
  '("a0" "a1" "a2" "a3" "a4" "a5" "a6"
    "b3" "b4" "b5" "b6"
    "ledger" "legal" "letter" "folio"))

(define (get-default-paper-size-bis)
  (with psize (getenv "PAPERSIZE")
    (if (and psize (!= psize "")) psize
        (with papersizefile (or (getenv "PAPERCONF") "/etc/papersize")
          (and (url-test? papersizefile "r")
               (with pps-port (open-input-file papersizefile)
                 (with size (read-line pps-port)
                   (close-input-port pps-port)
                   size)))))))

(tm-define (correct-paper-size s)
  (if (and (string? s) (in? s supported-sizes)) s "a4"))

(tm-define (standard-paper-size s)
  (if (and (string? s) (in? s standard-sizes)) s "user"))

(tm-define (get-default-paper-size)
  (correct-paper-size (get-default-paper-size-bis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define preview-command "default")

(define (notify-preview-command var val)
  (set! preview-command val))

(define (notify-printing-command var val)
  (set-printing-command val))

(define (notify-paper-type var val)
  (set-printer-paper-type (locase-first val)))

(define (notify-printer-dpi var val)
  (set-printer-dpi val))

(define-preferences
  ("native pdf" "on" noop)
  ("native postscript" "on" noop)
  ("texmacs->pdf:expand slides" "off" noop)
  ("texmacs->pdf:check" "off" noop)
  ("preview command" "default" notify-preview-command)
  ("printing command" (get-default-printing-command) notify-printing-command)
  ("paper type" (get-default-paper-size) notify-paper-type)
  ("printer dpi" "1200" notify-printer-dpi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing wrapper for slides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wrapped-print-to-file fname)
  (if (screens-buffer?)
      (let* ((cur (current-buffer))
             (buf (buffer-new)))
        (buffer-copy cur buf)
        (buffer-set-master buf cur)
        (switch-to-buffer buf)
        (set-drd cur)
        (dynamic-make-slides)
        (print-to-file fname)
        (switch-to-buffer cur)
        (buffer-close buf))
      (print-to-file fname)))

(tm-define (wrapped-print-to-pdf-embeded-with-tm fname)
    (unless (string=? (url-suffix fname) "pdf")
      (texmacs-error "Wrapped-print-to-pdf-embeded-with-tm" "fname is not a pdf"))
    (if (screens-buffer?)
      (let* ((cur (current-buffer))
             (buf (buffer-new)))
        (buffer-copy cur buf)
        (buffer-set-master buf cur)
        (switch-to-buffer buf)
        (set-drd cur)
        (dynamic-make-slides)
        (print-to-file fname)
        (unless (attach-doc-to-exported-pdf fname)
          (notify-now "Fail to attach tm to pdf"))
        (switch-to-buffer cur)
        (buffer-close buf))
      (begin
      (print-to-file fname)
      (unless (attach-doc-to-exported-pdf fname)
          (notify-now "Fail to attach tm to pdf")))))

(tm-define (attach-doc-to-exported-pdf fname)
  (let* ((tem-url (buffer-new))
         (new-url (url-relative tem-url (string-append (url-basename fname) ".tm")))
         (cur-url (current-buffer-url))
         (cur-tree (buffer-get cur-url))
         (linked-file (pdf-get-linked-file-paths cur-tree cur-url))
         (linked-file-with-main (array-url-append new-url linked-file))
         (new-tree (pdf-replace-linked-path cur-tree cur-url)))
    (buffer-rename tem-url new-url)
    (buffer-copy cur-url new-url)
    ;; copy also attachments and auxiliary data
    (with-buffer cur-url
      (let* ((attl (list-attachments)) 
             (atts (map get-attachment attl))
             (auxl (list-auxiliaries))
             (auxs (map get-auxiliary auxl)))
        (with-buffer new-url
          (for-each set-attachment attl atts)
          (for-each set-auxiliary auxl auxs))))
    (buffer-save new-url)
    (pdf-make-attachments fname linked-file-with-main fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (propose-postscript-name)
  (with name (propose-name-buffer)
    (if (string-ends? name ".tm")
	(string-append (string-drop-right name 3) ".ps")
	name)))

(tm-property (print-to-file name)
  (:synopsis "Print to file")
  (:argument name print-file "File name")
  (:default  name (propose-postscript-name)))

(tm-property (print-pages first last)
  (:synopsis "Print page selection")
  (:argument  first "First page")
  (:proposals first (list "1" ""))
  (:argument  last "Last page")
  (:proposals last  (list (number->string (get-page-count)) "")))

(tm-property (print-pages-to-file name first last)
  (:synopsis "Print page selection to file")
  (:argument  name print-file "File name")
  (:default   name (propose-postscript-name))
  (:argument  first "First page")
  (:proposals first (list "1" ""))
  (:argument  last "Last page")
  (:proposals last  (list (number->string (get-page-count)) "")))

(tm-define (preview-file u)
  (with s (url-sys-concretize u)
    (cond ((!= preview-command "default")
           (shell (string-append preview-command " " s " &")))
          ((or (os-mingw?) (os-win32?))
           (shell (string-append "cmd /c start " s)))
          ((os-macos?)
           (shell (string-append "open " s)))
          ((url-exists-in-path? "xdg-open")
           (shell (string-append "xdg-open " s)))
          ((url-exists-in-path? "ggv")
           (shell (string-append "ggv " s " &")))
          ((url-exists-in-path? "ghostview")
           (shell (string-append "ghostview " s " &")))
          ((url-exists-in-path? "gv")
           (shell (string-append "gv " s " &")))
          (else (set-message
                 "Error: ghostview does not seem to be installed on your system"
                 "preview")))))

(tm-define (preview-buffer)
  (with file (cond ((os-mingw?)
                    (let* ((p (getenv "TEXMACS_HOME_PATH"))
                           (f (string-append p "\\system\\tmp\\preview.pdf")))
                      (system->url f)))
                   ((or (os-macos?) (get-boolean-preference "native pdf"))
                    "$TEXMACS_HOME_PATH/system/tmp/preview.pdf")
                   (else "$TEXMACS_HOME_PATH/system/tmp/preview.ps"))
    (print-to-file file)
    (preview-file file)))

(tm-define (choose-file-and-print-page-selection start end)
  (:argument  start "First page")
  (:proposals start (list "1" ""))
  (:argument  end "Last page")
  (:proposals end (list (number->string (get-page-count)) ""))
  (choose-file (lambda (name) (print-pages-to-file name start end))
	       "Print page selection to file" "postscript"))
