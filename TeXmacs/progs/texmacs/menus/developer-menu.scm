
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : developer-menu.scm
;; DESCRIPTION : Menu items for developer mode
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; Things to do:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus developer-menu))

(use-modules (prog scheme-tools) (prog scheme-menu)
             (doc apidoc) (doc apidoc-widgets)
             (language natural)
             ;; Experimental (should be put elsewhere)
             (generic format-tools)
             (texmacs menus preferences-tools))

(define (scm-load-buffer u)
   (load-document u) 
   (if (not (url-exists? u)) 
;; save empty file & reload so that it is recognized as scheme code, not plain tm doc  
      (begin (buffer-save u) (revert-buffer-revert))))

(menu-bind developer-menu
  (group "Scheme")
  (link scheme-menu)
  ---
  (group "Translations")
  (link translations-menu)
  ---
  (group "Documentation")
  (link apidoc-menu)
  ---
  (group "Configuration")
  ((replace "Open %1" (verbatim "my-init-texmacs.scm"))
   (scm-load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm")))
  ((replace "Open %1" (verbatim "my-init-buffer.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm")))
  ((replace "Open %1" (verbatim "preferences.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/system/preferences.scm")))
  (assuming (side-tools?)
    ---
    (group "Experimental side tools")
    ("Context" (tool-toggle "context"))
    ("Invalid" (tool-toggle "invalid"))
    (-> "Test"
        ("Sections" (tool-toggle "sections"))
        ("Subsections" (tool-toggle "subsections")))
    (-> "Format"
        ("Paragraph" (tool-toggle "format paragraph")))
    (-> "Document"
        ("Paragraph" (tool-toggle "document paragraph"))
        ("Page" (tool-toggle "document page"))
        ("Margins" (tool-toggle "document margins"))
        ("Headers" (tool-toggle "document headers")))
    (-> "Preferences"
        ("General" (tool-toggle "general preferences"))
        ("Keyboard" (tool-toggle "keyboard preferences"))
        ("Mathematics" (tool-toggle "math preferences"))
        (-> "Converters"
            ("Html" (tool-toggle "html preferences"))
            ("LaTeX" (tool-toggle "latex preferences"))
            ("BibTeX" (tool-toggle "bibtex preferences"))
            ("Verbatim" (tool-toggle "verbatim preferences"))
            ("Pdf" (tool-toggle "pdf preferences"))
            ("Images" (tool-toggle "image preferences")))
        ("Security" (tool-toggle "security preferences"))
        ("Miscellaneous" (tool-toggle "miscellaneous preferences"))
        ("Experimental" (tool-toggle "experimental preferences")))))
