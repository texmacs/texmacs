
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
  (:use (texmacs texmacs tm-files)))

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
    "user"))

(define (get-default-paper-size-bis)
  (with psize (getenv "PAPERSIZE")
    (if (and psize (!= psize "")) psize
        (with papersizefile (or (getenv "PAPERCONF") "/etc/papersize")
          (and (access? papersizefile R_OK)
               (with pps-port (open-input-file papersizefile)
                 (with size (read-line pps-port)
                   (close-input-port pps-port)
                   size)))))))

(tm-define (correct-paper-size s)
  (if (and (string? s) (in? s supported-sizes)) s "a4"))

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

(define notify-font-type-flag #f)
(define (notify-font-type var val)
  (when notify-font-type-flag
    (with font-cache "$TEXMACS_HOME_PATH/system/cache/font_cache.scm"
      (system-remove (string->url font-cache)))
    (set-message "Restart in order to let new font type take effect"
		 "font type"))
  (set! notify-font-type-flag #t)
  (with type
      (cond ((== val "Metafont only") 0)
	    ((== val "Metafont + Type 1") 1)
	    ((== val "Type 1 + Metafont") 2)
	    ((== val "Type 1 only") 3)
	    (else 2))
    (set-font-type type)))

(define-preferences
  ("preview command" "default" notify-preview-command)
  ("printing command" "lpr" notify-printing-command)
  ("paper type" (get-default-paper-size) notify-paper-type)
  ("printer dpi" "600" notify-printer-dpi)
  ("font type" "Type 1 + Metafont" notify-font-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (propose-postscript-name)
  (with name (propose-name-buffer)
    (if (string-ends? name ".tm")
	(string-append (string-drop-right name 3) ".ps")
	name)))

(tm-property (print-to-file name)
  (:argument name print-file "File name")
  (:default  name (propose-postscript-name)))

(tm-property (print-pages first last)
  (:argument first "First page")
  (:argument last "Last page"))

(tm-property (print-pages-to-file name first last)
  (:argument name print-file "File name")
  (:default  name (propose-postscript-name))
  (:argument first "First page")
  (:argument last "Last page"))

(tm-define (preview-buffer)
  (print-to-file "$TEXMACS_HOME_PATH/system/tmp/preview.ps")
  (cond ((!= preview-command "default")
	 (shell (string-append preview-command
			       " $TEXMACS_HOME_PATH/system/tmp/preview.ps &")))
        ((os-win32?)
	 (shell "__previewps__ $TEXMACS_HOME_PATH/system/tmp/preview.ps"))
        ((os-macos?)
         (shell "open $TEXMACS_HOME_PATH/system/tmp/preview.ps"))
        ((url-exists-in-path? "ggv")
	 (shell "ggv $TEXMACS_HOME_PATH/system/tmp/preview.ps &"))
	((url-exists-in-path? "ghostview")
	 (shell "ghostview $TEXMACS_HOME_PATH/system/tmp/preview.ps &"))
	((url-exists-in-path? "gv")
	 (shell "gv $TEXMACS_HOME_PATH/system/tmp/preview.ps &"))
	(else (set-message
	       "Error: ghostview does not seem to be installed on your system"
	       "preview"))))

(tm-define (choose-file-and-print-page-selection start end)
  (:argument start "First page")
  (:argument end "Last page")
  (choose-file (lambda (name) (print-pages-to-file name start end))
	       "Print page selection to file" "postscript"))
