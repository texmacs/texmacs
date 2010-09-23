
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : main-menu.scm
;; DESCRIPTION : the default main menu of TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus main-menu)
  (:use (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main dynamic, extensible or user defined submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-menu)
(menu-bind texmacs-extra-icons)
(tm-define (buffer-menu) (get-buffer-menu))
(tm-define (project-buffer-menu) (get-project-buffer-menu))
(tm-define (style-menu) (get-style-menu))
(tm-define (add-package-menu) (get-add-package-menu))
(tm-define (remove-package-menu) (get-remove-package-menu))
(menu-bind bookmarks-menu)
(menu-bind test-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs main menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-menu
  (=> "File" (link file-menu))
  (=> "Edit" (link edit-menu))
  (if (not (in-graphics?)) (=> "Insert" (link insert-menu)))
  (if (in-source?) (=> "Source" (link source-menu)))
  (if (in-text?) (=> "Text" (link text-menu)))
  (if (in-math?) (=> "Mathematics" (link math-menu)))
  (if (in-session?) (=> "Session" (link session-menu)))
  (if (in-graphics?) (=> "Graphics" (link graphics-menu)))
  (if (in-table?) (link vertical-table-cell-menu))
  (if (with-linking-tool?)
      (=> "Link" (link link-menu)))
  (link texmacs-extra-menu)
  (if (not (in-graphics?)) (=> "Format" (link format-menu)))
  (=> "Document" (link document-menu))
  (if (and (not (project-attached?))
	   (== (get-init-tree "sectional-short-style") (tree 'macro "false")))
      (=> "Part" (link document-part-menu)))
  (if (project-attached?) (=> "Project" (link project-menu)))
  (if (with-versioning-tool?)
      (=> "Version" (link version-menu)))
  (=> "View" (link view-menu))
  (=> "Go" (link go-menu))
  (if (detailed-menus?) (=> "Tools" (link tools-menu)))
  (if (with-remote-connections?)
      (=> "Remote" (link remote-menu)))
  (if (with-debugging-tool?)
      (=> "Debug" (link debug-menu)))
  (if (nnull? (test-menu))
      (=> "Test" (link test-menu)))
  (=> "Help" (link help-menu)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs popup menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-popup-menu
  (-> "File" (link file-menu))
  (-> "Edit" (link edit-menu))
  (if (not (in-graphics?)) (-> "Insert" (link insert-menu)))
  (if (in-source?) (-> "Source" (link source-menu)))
  (if (in-text?) (-> "Text" (link text-menu)))
  (if (in-math?) (-> "Mathematics" (link math-menu)))
  (if (in-graphics?) (-> "Graphics" (link graphics-menu)))
  (if (in-session?) (-> "Session" (link session-menu)))
  (if (in-table?) (link horizontal-table-cell-menu))
  (if (with-linking-tool?) (-> "Link" (link link-menu)))
  (if (not (in-graphics?)) (-> "Format" (link format-menu)))
  (-> "Document" (link document-menu))
  (if (== (get-init-tree "sectional-short-style") (tree 'macro "false"))
      (-> "Part" (link document-part-menu)))
  (if (project-attached?) (=> "Project" (link project-menu)))
  (if (with-versioning-tool?) (-> "Version" (link version-menu)))
  (-> "View" (link view-menu))
  (-> "Go" (link go-menu))
  (if (detailed-menus?) (-> "Tools" (link tools-menu)))
  (if (with-remote-connections?) (-> "Remote" (link remote-menu)))
  (if (with-debugging-tool?) (=> "Debug" (link debug-menu)))
  (if (nnull? (test-menu)) (=> "Test" (link test-menu)))
  ---
  (-> "Help" (link help-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-main-icons
  (=> (balloon (icon "tm_new.xpm") "Create a new document")
      (link new-file-menu))
  (=> (balloon (icon "tm_open.xpm") "Load a file") (link load-menu))
  (=> (balloon (icon "tm_save.xpm") "Save this buffer") (link save-menu))
  (=> (balloon (icon "tm_print.xpm") "Print") (link print-menu))
  (if (detailed-menus?)
      (=> (balloon (icon "tm_style.xpm") "Select a document style")
	  (link document-style-menu))
      (=> (balloon (icon "tm_language.xpm") "Select a language")
	  (link global-language-menu)))
  (=> (balloon (icon "tm_cancel.xpm") "Close") (link close-menu))
  |
  ((balloon (icon "tm_cut.xpm") "Cut text")
   (clipboard-cut "primary"))
  ((balloon (icon "tm_copy.xpm") "Copy text")
   (clipboard-copy "primary"))
  ((balloon (icon "tm_paste.xpm") "Paste text")
   (clipboard-paste "primary"))
  (if (not (in-search-mode?))
      ((balloon (icon "tm_find.xpm") "Find text") (search-start #t)))
  (if (in-search-mode?)
      ((balloon (icon "tm_find_next.xpm") "Find next match")
       (search-button-next)))
  ((balloon (icon "tm_replace.xpm") "Query replace")
   (interactive replace-start-forward))
  ((balloon (icon "tm_spell.xpm") "Check text for spelling errors")
   (spell-start))
  ((balloon (icon "tm_undo.xpm") "Undo last changes") (undo 0))
  ((balloon (icon "tm_redo.xpm") "Redo undone changes") (redo 0))
  (if (not (in-graphics?))
      |
      (=> (balloon (icon "tm_table.xpm") "Insert a table")
	  (link insert-table-menu))
      (=> (balloon (icon "tm_image.xpm") "Insert a picture")
	  (link insert-image-menu))
      (=> (balloon (icon "tm_link.xpm") "Insert a link")
	  (link insert-link-menu))
      (if (detailed-menus?)
	  (if (style-has? "std-fold-dtd")
	      (=> (balloon (icon "tm_switch.xpm") "Switching and folding")
		  (link insert-fold-menu)))
	  (=> (balloon (icon "tm_animate.xpm") "Animation")
	      (link insert-animation-menu)))
      (=> (balloon (icon "tm_math.xpm") "Insert mathematics")
	  (link insert-math-menu))
      (if (and (style-has? "program-dtd") (detailed-menus?))
	  (=> (balloon (icon "tm_shell.xpm")
		       "Start an interactive session")
	      (link insert-session-menu))))
  |
  ((balloon (icon "tm_back.xpm") "Browse back (C-<)")
   (cursor-history-backward))
  ((balloon (icon "tm_reload.xpm") "Reload (C-F2)")
   (revert-buffer))
  ((balloon (icon "tm_forward.xpm") "Browse forward (C->)")
   (cursor-history-forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The context dependent icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-context-icons
  (if (in-source?) (link source-icons))
  (if (in-text?) (link text-icons))
  (if (in-session?) (link session-icons))
  (if (in-math?) (link math-icons))
  (if (in-graphics?) (link graphics-icons))
  (if (not (in-graphics?)) |)
  (if (or (in-source?) (in-text?)) (link text-format-icons))
  (if (in-math?) (link math-format-icons))
  (if (in-prog?) (link prog-format-icons))
  (if (in-table?) (link table-icons))
  (link help-icons))
