
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file-menu.scm
;; DESCRIPTION : the file menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus file-menu)
  (:use
    (utils library cursor)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-files)
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
    `(,import-text (choose-file ,routine ,load-text ,(rw fm)))))

(define (import-item* fm name)
  (define (rw s) (if (== s "verbatim") "" s))
  (let* ((load-text (string-append "Load " (string-downcase name) " file"))
	 (routine `(buffer-loader ,fm)))
    `(,name (choose-file ,routine ,load-text ,(rw fm)))))

(define-macro (import-menu-promise flag?)
  `(menu-dynamic
     ,@(converter-to-menu "texmacs-file" "-file" #f
			  (if flag? import-item import-item*))))

(define (export-item fm name)
  (let* ((export-text (string-append "Export as#" name))
	 (load-text (string-append "Save " (string-downcase name) " file"))
	 (routine `(buffer-saver ,fm)))
    `(,export-text (choose-file ,routine ,load-text ,fm))))

(define (export-item* fm name)
  (let* ((load-text (string-append "Save " (string-downcase name) " file"))
	 (routine `(buffer-saver ,fm)))
    `(,name (choose-file ,routine ,load-text ,fm))))

(define-macro (export-menu-promise flag?)
  `(menu-dynamic
     ,@(converter-from-menu "texmacs-file" "-file" #f
			    (if flag? export-item export-item*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the File menu and for the icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind new-file-menu
  ("New document" (new-buffer))
  ("Open new window" (open-window))
  ("Clone window" (clone-window)))

(menu-bind load-menu
  ("Load" (open-buffer))
  ("Revert" (revert-buffer))
  ("Load in new window" (choose-file load-in-new-window "Load file" ""))
  ---
  (promise (import-menu-promise #t)))

(menu-bind save-menu
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ---
  (promise (export-menu-promise #t))
  ---
  ("Export as#Pdf" (choose-file print-to-file "Save pdf file" "pdf"))
  ("Export as#PostScript"
   (choose-file print-to-file "Save postscript file" "postscript")))

(menu-bind print-menu
  ("Preview with ghostview" (preview-with-ghostview))
  ---
  ("Print all" (print-buffer))
  ("Print page selection" (interactive print-pages))
  ("Print all to file"
   (choose-file print-to-file "Print all to file" "postscript"))
  ("Print page selection to file"
   (interactive choose-file-and-print-page-selection)))

(menu-bind close-menu
  ("Close document" (safely-kill-buffer))
  ("Close window" (safely-kill-window))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The File menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind file-menu
  ("New" (new-buffer))
  ("Load" (open-buffer))
  ;("Load in new window" (choose-file "Load file" "" 'load-in-new-window))
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ("Revert" (revert-buffer))
  ---
  (-> "Page setup" (link page-setup-menu))
  (-> "Print" (link print-menu))
  (-> "Import"
      (promise (import-menu-promise #f)))
  (-> "Export"
      (promise (export-menu-promise #f))
      ---
      ("Pdf" (choose-file print-to-file "Save pdf file" "pdf"))
      ("PostScript"
       (choose-file print-to-file "Save postscript file" "postscript")))
  ---
  ("Close document" (safely-kill-buffer))
  ("Close TeXmacs" (safely-quit-TeXmacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Go menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind go-menu
  (when (cursor-has-history?)
    ("Back" (cursor-history-backward)))
  (when (cursor-has-future?)
    ("Forward" (cursor-history-forward)))
  ---
  (link buffer-menu)
  (if (nnull? (bookmarks-menu))
      ---
      (link bookmarks-menu)))
