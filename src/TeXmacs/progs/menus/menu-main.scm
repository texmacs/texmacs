
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-main.scm
;; DESCRIPTION : the default main menu of TeXmacs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main dynamic, extensible or user defined submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-menu)
(menu-bind texmacs-extra-icons)
(menu-bind style-menu)
(menu-bind use-package-menu)
(menu-bind bookmarks-menu)
(menu-bind test-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs main menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-menu
  (=> "File" (link file-menu))
  (=> "Edit" (link edit-menu))
  (if (not (in-graphics?)) (=> "Insert" (link insert-menu)))
  (if (in-text?) (=> "Text" (link text-menu)))
  (if (in-math?) (=> "Mathematics" (link mathematics-menu)))
  (if (in-io?) (=> "Session" (link session-main-menu)))
  (if (in-graphics?) (=> "Graphics" (link graphics-menu)))
  (if (in-table?) (=> "Table" (link table-menu)))
  (link texmacs-extra-menu)
  (if (not (in-graphics?)) (=> "Format" (link format-menu)))
  (=> "Document" (link document-menu))
  (if (project-attached?) (=> "Project" (link project-menu)))
  (=> "View" (link view-menu))
  (=> "Go" (link go-menu))
  (=> "Tools" (link tools-menu))
  (=> "Help" (link help-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs popup menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-popup-menu
  (-> "File" (link file-menu))
  (-> "Edit" (link edit-menu))
  (if (not (in-graphics?)) (-> "Insert" (link insert-menu)))
  (if (in-text?) (-> "Text" (link text-menu)))
  (if (in-math?) (-> "Mathematics" (link mathematics-menu)))
  (if (in-graphics?) (-> "Graphics" (link graphics-menu)))
  (if (in-io?) (-> "Session" (link session-main-menu)))
  (if (in-table?) (-> "Table" (link table-menu)))
  (if (not (in-graphics?)) (-> "Format" (link format-menu)))
  (-> "Document" (link document-menu))
  (if (project-attached?) (=> "Project" (link project-menu)))
  (-> "View" (link view-menu))
  (-> "Go" (link go-menu))
  (-> "Tools" (link tools-menu))
  ---
  (-> "Help" (link help-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-main-icons
  (=> (balloon (icon "tm_new.xpm") "Open a new buffer") (link new-file-menu))
  (=> (balloon (icon "tm_open.xpm") "Load a file") (link load-menu))
  (=> (balloon (icon "tm_save.xpm") "Save this buffer") (link save-menu))
  (=> (balloon (icon "tm_print.xpm") "Print") (link print-menu))
  (=> (balloon (icon "tm_style.xpm") "Select a document style")
      (link document-style-menu))
  (=> (balloon (icon "tm_language.xpm") "Select a language")
      (link global-language-menu))
  (=> (balloon (icon "tm_cancel.xpm") "Close") (link close-menu))
  |
  ((balloon (icon "tm_cut.xpm") "Cut text#(C-w)")
   (clipboard-cut "primary"))
  ((balloon (icon "tm_copy.xpm") "Copy text#(M-w)")
   (clipboard-copy "primary"))
  ((balloon (icon "tm_paste.xpm") "Paste text#(C-y)")
   (clipboard-paste "primary"))
  (if (not (in-search-mode?))
      ((balloon (icon "tm_find.xpm") "Find text#(C-s)") (search-start #t)))
  (if (in-search-mode?)
      ((balloon (icon "tm_find_next.xpm") "Find next match#(C-s)")
       (search-button-next)))
  ((balloon (icon "tm_replace.xpm") "Query replace#(C-=)")
   (interactive '("Replace:" "Replace by:") 'replace-start-forward))
  ((balloon (icon "tm_spell.xpm") "Check text for spelling errors#(M-$)")
   (spell-start))
  (if (not (help-buffer?))
      ((balloon (icon "tm_undo.xpm") "Undo last changes#(M-[)") (undo))
      ((balloon (icon "tm_redo.xpm") "Redo undone changes#(M-])") (redo)))
  (if (help-buffer?)
      ((balloon (icon "tm_back.xpm") "Browse back") (browse-help -1))
      ((balloon (icon "tm_forward.xpm") "Browse forward") (browse-help 1)))
  (if (not (in-graphics?))
      |
      (=> (balloon (icon "tm_table.xpm") "Insert a table")
	  (link insert-table-menu))
      (=> (balloon (icon "tm_image.xpm") "Insert a picture")
	  (link insert-image-menu))
      (=> (balloon (icon "tm_link.xpm") "Insert a link")
	  (link insert-link-menu))
      (=> (balloon (icon "tm_switch.xpm") "Switching and folding")
	  (link insert-switch-menu))
      (=> (balloon (icon "tm_math.xpm") "Insert mathematics")
	  (link insert-mathematics-menu))
      (if (style-has? "program-dtd")
	  (=> (balloon (icon "tm_shell.xpm")
		       "Start an interactive session")
	      (link insert-session-menu)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The context dependent icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-context-icons
  (if (in-text?) (link texmacs-text-icons))
  (if (in-io?) (link texmacs-session-icons))
  (if (in-math?) (link texmacs-math-icons))
  (if (in-graphics?) (link texmacs-graphics-icons))
  (if (not (in-graphics?)) |)
  (if (in-prog?) (link prog-modifier-icons))
  (if (in-text?) (link text-modifier-icons))
  (if (in-math?) (link math-modifier-icons))
  (if (not (in-graphics?))
      (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
	  (link color-menu)))
  (if (in-table?) (link texmacs-table-icons))
  (link texmacs-help-icons))
