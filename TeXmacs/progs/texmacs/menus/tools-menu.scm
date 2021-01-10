
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tools-menu.scm
;; DESCRIPTION : the tools menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus tools-menu)
  (:use (texmacs texmacs tm-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (clipboard-preference-menu cvs fun)
  (with l (cvs "texmacs-file" "-file" #f)
    (for (fm l)
      (with name (format-get-name fm)
        ((eval name) (fun fm))))))

(tm-define (clipboard-import-preference-menu)
  (clipboard-preference-menu converters-to-special clipboard-set-import))
(tm-define (clipboard-export-preference-menu)
  (clipboard-preference-menu converters-from-special clipboard-set-export))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Tools menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind tools-menu
  (-> "Macros"
      (link source-macros-menu))
  (-> "Keyboard"
      ("Edit keyboard shortcuts" (open-shortcuts-editor "" "")))
  ---
  (-> "Update"
      ("Inclusions" (inclusions-gc))
      ("Plugins" (reinit-plugin-cache))
      ("Styles" (style-clear-cache)))
  (if (url-exists-in-path? "pdflatex")
      (-> "LaTeX"
          (link tmtex-menu)))
  (-> "References"
      (link ref-menu))
  (if supports-email?
      (-> "Email"
          ("Open mailbox" (email-open-mailbox))
          ("Retrieve email" (begin (email-pop) (email-open-inbox)))
          ---
          ("Pop server settings" (interactive email-settings))))
  (-> "Project"
      (link project-manage-menu))
  (-> "Statistics"
      ("Count characters" (show-character-count))
      ("Count words" (show-word-count))
      ("Count lines" (show-line-count)))
  ---
  ("Create web site" (open-website-builder))
  ;;(-> "Web"
  ;;    ("Create web site" (tmweb-interactive-build))
  ;;    ("Update web site" (tmweb-interactive-update)))
  (-> "Fonts"
      ("Look for more fonts"
       (system-wait "Full search for more fonts on your system"
                    "(can be long)")
       (font-database-build-local))
      ("Clear font cache" (clear-font-cache)))
  (-> "Miscellaneous"
      ("Clear undo history" (clear-undo-history))
      ("Save auxiliary data" (toggle-save-aux))
      ("Show key presses" (toggle-show-kbd))
      ---
      (-> "Import selections as"
          (link clipboard-import-preference-menu))
      (-> "Export selections as"
          (link clipboard-export-preference-menu)))
  ---
  ("Database tool" (toggle-preference "database tool"))
  ("Debugging tool" (toggle-preference "debugging tool"))
  ("Developer tool" (toggle-preference "developer tool"))
  ("Linking tool" (toggle-preference "linking tool"))
  ("Presentation tool" (toggle-preference "presentation tool"))
  ("Remote tool" (toggle-preference "remote tool"))
  ("Source macros tool" (toggle-preference "source tool"))
  ("Versioning tool" (toggle-preference "versioning tool")))
