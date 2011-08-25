
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

(texmacs-module (texmacs menus tools-menu))

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
  (-> "Execute"
      ("Execute system command" (interactive system))
      ("Evaluate scheme expression" (interactive footer-eval)))
  (-> "Selections"
      (-> "Import" (link clipboard-import-preference-menu))
      (-> "Export" (link clipboard-export-preference-menu)))
  (-> "Update"
      ("Image links" (image-gc))
      ("Inclusions" (inclusions-gc))
      ("Styles" (style-clear-cache)))
  (-> "Web"
      ("Create web site" (tmweb-interactive-build)))
  (-> "Project"
      (link project-manage-menu))
  (-> "Miscellaneous"
      ("Clear undo history" (clear-undo-history)))
  ---
  ("Debugging tool" (toggle-preference "debugging tool"))
  ("Linking tool" (toggle-preference "linking tool"))
  ("Presentation tool" (toggle-preference "presentation tool"))
  ("Source macros tool" (toggle-preference "source tool"))
  ("Versioning tool" (toggle-preference "versioning tool")))
