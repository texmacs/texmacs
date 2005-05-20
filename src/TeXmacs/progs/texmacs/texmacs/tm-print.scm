
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-print.scm
;; DESCRIPTION : routines for printing documents
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

(tm-define (get-default-paper-size)
  (or (getenv "PAPERSIZE")
      (let ((papersizefile (or (getenv "PAPERCONF") '"/etc/papersize")))
	(if
	 (access? papersizefile R_OK)
	 (let ((pps-port (open-input-file papersizefile)))
	   (let ((size (read-line pps-port)))
	     (begin
	       (close-input-port pps-port)
	       size)))
	 "a4"))))

(define (get-default-font-setting)
  (cond ((support-ec-fonts?) "EC fonts")
        ((os-win32?) "True Type")
        (else "CM fonts")))

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

(define (notify-font-type var val)
  (with type
      (cond ((== val "EC fonts") 0)
	    ((== val "CM fonts") 1)
	    ((== val "Bitmap") (if (support-ec-fonts?) 0 1))
	    ((== val "True Type") 2)
	    ((== val "True type") 2)
	    (else 1))
    (set-font-type type)))

(define-preferences
  ("preview command" "default" notify-preview-command)
  ("printing command" "lpr" notify-printing-command)
  ("paper type" (get-default-paper-size) notify-paper-type)
  ("printer dpi" "600" notify-printer-dpi)
  ("font type" (get-default-font-setting) notify-font-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (propose-postscript-name)
  (with name (propose-name-buffer)
    (if (string-ends? name ".tm")
	(string-append (string-drop-right name 3) ".ps")
	name)))

(tm-property (print-to-file name)
  (:argument name "File name")
  (:default  name (propose-postscript-name)))

(tm-property (print-pages first last)
  (:argument first "First page")
  (:argument last "Last page"))

(tm-property (print-pages-to-file name first last)
  (:argument name "File name")
  (:default  name (propose-postscript-name))
  (:argument first "First page")
  (:argument last "Last page"))

(tm-define (preview-with-ghostview)
  (print-to-file "$TEXMACS_HOME_PATH/system/tmp/preview.ps")
  (cond ((!= preview-command "default")
	 (shell (string-append preview-command
			       " $TEXMACS_HOME_PATH/system/tmp/preview.ps &")))
        ((os-win32?)
	 (shell "__previewps__ $TEXMACS_HOME_PATH/system/tmp/preview.ps"))
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
  (choose-file (lambda (name) (print-pages-to-file name ,start ,end))
	       "Print page selection to file" "postscript"))
