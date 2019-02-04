
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
(menu-bind plugin-menu)
(menu-bind plugin-icons)
(tm-define (style-menu) (get-style-menu))
(tm-define (add-package-menu) (get-add-package-menu))
(tm-define (remove-package-menu) (get-remove-package-menu))
(tm-define (toggle-package-menu) (get-toggle-package-menu))
(menu-bind bookmarks-menu)
(menu-bind test-menu)
(menu-bind help-icons (if (in-session?) (link session-help-icons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Remote menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind remote-menu
  (if (and (null? remote-client-list) (not (server-started?)))
      (link start-client-menu)
      ;;---
      ;;(link start-server-menu)
      )
  (if (and (null? remote-client-list) (server-started?))
      (link server-menu))
  (if (nnull? remote-client-list)
      (link client-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs main menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-menu
  (=> "File" (link file-menu))
  (=> "Edit" (link edit-menu))
  (if (in-graphics?)
      (=> "Insert" (link graphics-insert-menu))
      (link texmacs-extra-menu)
      (=> "Focus" (link graphics-focus-menu)))
  (if (not (in-graphics?))
      (=> "Insert" (link insert-menu))
      (if (in-manual?)
          (=> "Manual" (link tmdoc-menu)))
      (if (or (in-source?) (with-source-tool?))
          (=> "Source" (link source-menu)))
      (if (with-linking-tool?)
          (=> "Link" (link link-menu)))
      (if (in-presentation?)
          (=> "Dynamic" (link dynamic-menu)))
      ;;(if (style-has? "course-style")
      ;;    (=> "Course" (link course-menu)))
      ;;(if (style-has? "exam-style")
      ;;    (=> "Exam" (link exam-menu)))
      (if (style-has? "icourse-dtd")
          (=> "Icourse" (link calc-icourse-menu)))
      (link plugin-menu)
      (link texmacs-extra-menu)
      (=> "Focus" (link focus-menu))
      (=> "Format" (link format-menu)))
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
  (if (with-database-tool?)
      (=> "Data" (link db-menu)))
  (if (with-remote-tool?)
      (=> "Remote" (link remote-menu)))
  (if (with-debugging-tool?)
      (=> "Debug" (link debug-menu)))
  (if (with-developer-tool?)
      (=> "Developer" (link developer-menu)))
  (if (nnull? (test-menu))
      (=> "Test" (link test-menu)))
  (=> "Help" (link help-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs popup menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-popup-menu
  (link focus-menu))

(menu-bind texmacs-alternative-popup-menu
  (-> "File" (link file-menu))
  (-> "Edit" (link edit-menu))
  (if (in-graphics?)
      (-> "Insert" (link graphics-insert-menu))
      (-> "Focus" (link graphics-focus-menu)))
  (if (not (in-graphics?))
      (-> "Insert" (link insert-menu))
      (if (in-manual?)
          (-> "Manual" (link tmdoc-menu)))
      (if (or (in-source?) (with-source-tool?))
          (-> "Source" (link source-menu)))
      (if (with-linking-tool?)
          (-> "Link" (link link-menu)))
      (if (in-presentation?)
          (-> "Dynamic" (link dynamic-menu)))
      ;;(if (style-has? "course-style")
      ;;    (-> "Course" (link course-menu)))
      ;;(if (style-has? "exam-style")
      ;;    (-> "Exam" (link exam-menu)))
      (if (style-has? "icourse-dtd")
          (-> "Icourse" (link calc-icourse-menu)))
      (-> "Focus" (link focus-menu))
      (-> "Format" (link format-menu)))
  (-> "Document" (link document-menu))
  (if (== (get-init-tree "sectional-short-style") (tree 'macro "false"))
      (-> "Part" (link document-part-menu)))
  (if (project-attached?) (=> "Project" (link project-menu)))
  (if (with-versioning-tool?) (-> "Version" (link version-menu)))
  (-> "View" (link view-menu))
  (-> "Go" (link go-menu))
  (if (detailed-menus?) (-> "Tools" (link tools-menu)))
  (if (with-database-tool?) (-> "Data" (link db-menu)))
  (if (with-remote-tool?) (-> "Remote" (link remote-menu)))
  (if (with-debugging-tool?) (-> "Debug" (link debug-menu)))
  (if (nnull? (test-menu)) (-> "Test" (link test-menu)))
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
  ((balloon (icon "tm_build.xpm") "Update this buffer")
   (update-document "all"))
  (=> (balloon (icon "tm_print.xpm") "Print") (link print-menu-inline))
  (if (use-menus?)
      (=> (balloon (icon "tm_preferences.xpm") "Change the TeXmacs preferences")
          (link preferences-menu)))
  (if (use-popups?)
      ((balloon (icon "tm_preferences.xpm") "Change the TeXmacs preferences")
       (open-preferences)))
  (if (detailed-menus?)
      ;;(=> (balloon (icon "tm_style.xpm") "Select a document style")
      ;;(link document-style-menu))
      ;;(=> (balloon (icon "tm_language.xpm") "Select a language")
      ;;(link global-language-menu))
      )
  (=> (balloon (icon "tm_cancel.xpm") "Close") (link close-menu))
  /
  ((balloon (icon "tm_cut.xpm") "Cut text")
   (clipboard-cut "primary"))
  ((balloon (icon "tm_copy.xpm") "Copy text")
   (clipboard-copy "primary"))
  ((balloon (icon "tm_paste.xpm") "Paste text")
   (clipboard-paste "primary"))
  ((balloon (icon "tm_find.xpm") "Find text")
   (interactive-search))
  ((balloon (icon "tm_replace.xpm") "Query replace")
   (interactive-replace))
  (if (not (in-math?))
      ((balloon (icon "tm_spell.xpm") "Check text for spelling errors")
       (interactive-spell)))
  (if (in-math?)
      (=> (balloon (icon "tm_spell.xpm") "Correct mathematical formulas")
          (link math-correct-menu)))
  ((balloon (icon "tm_undo.xpm") "Undo last changes") (undo 0))
  ((balloon (icon "tm_redo.xpm") "Redo undone changes") (redo 0))
  /
  ((balloon (icon "tm_back.xpm") "Browse back")
   (cursor-history-backward))
  ((balloon (icon "tm_reload.xpm") "Reload")
   (revert-buffer))
  ((balloon (icon "tm_forward.xpm") "Browse forward")
   (cursor-history-forward))
  (if (in-presentation?)
    /
    (link dynamic-icons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs side tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (upward-context-trees t)
  (cond ((tree-is-buffer? t) (list t))
        ((tree-atomic? t) (upward-context-trees (tree-up t)))
        (else (cons t (upward-context-trees (tree-up t))))))

(tm-widget (texmacs-side-tool t)
  (horizontal
    (mini #t
      ((eval (symbol->string (tree-label t)))
       (tree-select t)))
    >>>))

(tm-widget (texmacs-side-tools)
  (for (t (upward-context-trees (cursor-tree)))
    (dynamic (texmacs-side-tool t))
    ===)
  (glue #t #t 100 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The TeXmacs bottom tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (texmacs-bottom-tools)
  (if (qt-gui?)
      (link texmacs-bottom-toolbars))
  (if (not (qt-gui?))
      (glue #f #f 0 2)
      (horizontal
        (link texmacs-bottom-toolbars))
      (glue #f #f 0 1)
      ---))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The mode dependent icon bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-mode-icons
  (if (in-source?) (link source-icons))
  (if (in-text?) (link text-icons))
  (if (in-math?) (link math-icons))
  (if (in-prog?) (link prog-icons))
  (if (in-graphics?) (link graphics-icons))
  (link help-icons))
