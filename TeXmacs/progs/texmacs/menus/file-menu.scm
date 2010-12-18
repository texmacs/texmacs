
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

(tm-menu (import-menu flag?)
  (with l (converters-to-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (load-text (string-append "Load " (string-downcase name) " file"))
             (import-text `(concat "Import " ,name))
             (text (if flag? import-text name))
             (format (if (== fm "varbatim") "" fm)))
        ((eval text) (choose-file (buffer-loader fm) load-text format))))))

(tm-define (import-top-menu) (import-menu #t))
(tm-define (import-import-menu) (import-menu #f))

(tm-menu (export-menu flag?)
  (with l (converters-from-special "texmacs-file" "-file" #f)
    (for (fm l)
      (let* ((name (format-get-name fm))
             (save-text (string-append "Save " (string-downcase name) " file"))
             (export-text `(concat "Export as " ,name))
             (text (if flag? export-text name)))
        ((eval text) (choose-file (buffer-saver fm) save-text fm))))))

(tm-define (export-top-menu) (export-menu #t))
(tm-define (export-export-menu) (export-menu #f))

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
  (link import-top-menu))

(menu-bind save-menu
  ("Save" (save-buffer))
  ("Save as" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ---
  (link export-top-menu)
  ---
  ((eval '(concat "Export as " "Pdf"))
   (choose-file print-to-file "Save pdf file" "pdf"))
  ((eval '(concat "Export as " "PostScript"))
   (choose-file print-to-file "Save postscript file" "postscript")))

(menu-bind print-menu
  ("Preview" (preview-buffer))
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
   (if (experimental-qt-gui?)
       ("Preview" (preview-buffer))
       ("Print" (interactive-print-buffer)))
  (if (not (experimental-qt-gui?))
      (-> "Print" (link print-menu)))
  (-> "Page setup" (link page-setup-menu))
  (-> "Import"
      (link import-import-menu))
  (-> "Export"
      (link export-export-menu)
      ---
      ("Pdf" (choose-file print-to-file "Save pdf file" "pdf"))
      ("Postscript"
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
