
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-file.scm
;; DESCRIPTION : the file menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-file)
  (:use
    (texmacs texmacs tm-server) (texmacs texmacs tm-files)
    (texmacs texmacs tm-print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: yet another bug in Guile: for some strange reason,
;; import-menu-promise and export-menu-promise are not
;; reevaluated at each run. This *is* the case for top level macros,
;; but apparently not for local macros or macros in a module :^(((

(define (import-item fm name)
  (define (rw s) (if (== s "verbatim") "" s))
  (let* ((import-text (string-append "Import#" name))
	 (load-text (string-append "Load " (string-downcase name) " file"))
	 (routine `(buffer-loader ,fm)))
    `(,import-text ... (choose-file ,load-text ,(rw fm) ',routine))))

(define (import-item* fm name)
  (define (rw s) (if (== s "verbatim") "" s))
  (let* ((load-text (string-append "Load " (string-downcase name) " file"))
	 (routine `(buffer-loader ,fm)))
    `(,name ... (choose-file ,load-text ,(rw fm) ',routine))))

(define-macro (import-menu-promise flag?)
  `(menu-dynamic
     ,@(converter-to-menu "texmacs-file" "-file" #f
			  (if flag? import-item import-item*))))

(define (export-item fm name)
  (let* ((export-text (string-append "Export as#" name))
	 (load-text (string-append "Save " (string-downcase name) " file"))
	 (routine `(buffer-saver ,fm)))
    `(,export-text ... (choose-file ,load-text ,fm ',routine))))

(define (export-item* fm name)
  (let* ((load-text (string-append "Save " (string-downcase name) " file"))
	 (routine `(buffer-saver ,fm)))
    `(,name ... (choose-file ,load-text ,fm ',routine))))

(define-macro (export-menu-promise flag?)
  `(menu-dynamic
     ,@(converter-from-menu "texmacs-file" "-file" #f
			    (if flag? export-item export-item*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the File menu and for the icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind new-file-menu
  ("New buffer" (new-buffer))
  ("Open new window" (open-window))
  ("Clone window" (clone-window)))

(menu-bind load-menu
  ("Load buffer" ... (choose-file "Load file" "" 'load-buffer))
  ("Revert buffer" ... (revert-buffer))
  ("Load in new window" ...
   (choose-file "Load file" "" 'load-in-new-window))
  ---
  (promise (import-menu-promise #t)))

(menu-bind save-menu
  ("Save buffer" (save-buffer))
  ("Save buffer as" ...
   (choose-file "Save TeXmacs file" "texmacs" 'save-buffer))
  ---
  (promise (export-menu-promise #t))
  ---
  ("Export as#Pdf" ...
   (choose-file "Save pdf file" "pdf" 'print-to-file))
  ("Export as#PostScript" ...
   (choose-file "Save postscript file" "postscript" 'print-to-file)))

(menu-bind print-menu
  ("Preview with ghostview" (preview-with-ghostview))
  ---
  ("Print all" (print))
  ("Print page selection" ...
   (interactive '("First page:" "Last page:") 'print-pages))
  ("Print all to file" ...
   (choose-file "Print all to file" "postscript" 'print-to-file))
  ("Print page selection to file" ...
   (interactive '("First page:" "Last page:")
		'choose-file-and-print-page-selection)))

(menu-bind close-menu
  ("Close buffer" (safely-kill-buffer))
  ("Close window" (safely-kill-window))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The File menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind file-menu
  ("New" (new-buffer))
  ("Load" ... (choose-file "Load file" "" 'load-buffer))
  ("Save" (save-buffer))
  ("Save as" ...
   (choose-file "Save TeXmacs file" "texmacs" 'save-buffer))
  ("Revert" ... (revert-buffer))
  ---
  (-> "Page setup" (link page-setup-menu))
  (-> "Print" (link print-menu))
  (-> "Import"
      (promise (import-menu-promise #f)))
  (-> "Export"
      (promise (export-menu-promise #f))
      ---
      ("Pdf" ...
       (choose-file "Save pdf file" "pdf" 'print-to-file))
      ("PostScript" ...
       (choose-file "Save postscript file" "postscript" 'print-to-file)))
  ---
  ("Close buffer" (safely-kill-buffer))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Go menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind go-menu
  (link buffer-menu)
  (if (not (null? (menu-get 'bookmarks-menu)))
      ---
      (link bookmarks-menu)))
