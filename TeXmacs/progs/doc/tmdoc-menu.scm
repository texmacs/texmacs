
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-menu.scm
;; DESCRIPTION : menus for writing TeXmacs documentation
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-menu)
  (:use (doc tmdoc-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for TeXmacs documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind tmdoc-meta-menu
  ("Title" (tmdoc-insert-title))
  ("Copyright" (tmdoc-insert-copyright))
  ("License" (tmdoc-insert-license))
  ("GNU FDL" (tmdoc-insert-gnu-fdl)))

(menu-bind tmdoc-traversal-menu
  ("Traverse" (make 'traverse))
  (when (inside? 'traverse)
    ("Branch" (tmdoc-make-branch 'branch))
    ("Extra branch" (tmdoc-make-branch 'extra-branch))
    ("Continue" (tmdoc-make-branch 'continue))))

(menu-bind tmdoc-explain-menu
  ("Explanatory item" (make 'explain))
  (when (inside? 'explain)
    ("TeXmacs macro" (make 'explain-macro))
    ("Environment variable" (make 'var-val))
    ("Synopsis" (tmdoc-insert-explain-synopsis))))

(menu-bind tmdoc-gui-menu
  ("Keyboard shortcut" (make 'shortcut))
  ("Explicit keystroke" (make 'key))
  ---
  ("Menu" (make 'menu))
  ("Submenu" (make 'submenu))
  ("Subsubmenu" (make 'subsubmenu))
  ("Subsubsubmenu" (make 'subsubsubmenu))
  ---
  ("Icon" (make 'icon))
  ("Screenshot" (make 'screenshot)))

(menu-bind tmdoc-annotate-menu
  ("Primitive or macro" (make 'markup))
  ("Macro argument" (make 'src-arg))
  ("Environment variable" (make 'src-var))
  ("Length" (make 'src-length))
  ---
  ("TeXmacs style" (make 'tmstyle))
  ("TeXmacs package" (make 'tmpackage))
  ("TeXmacs DTD" (make 'tmdtd)))

(menu-bind tmdoc-indication-menu
  ("Cursor" (make 'cursor))
  ("Small focus" (make 'small-focus))
  ("Big focus" (make 'big-focus))
  ("Small environment box" (make 'small-envbox))
  ("Big environment box" (make 'big-envbox)))

(menu-bind tmdoc-menu
  (when (in-text?)
    (-> "Meta data" (link tmdoc-meta-menu))
    (-> "Traversal" (link tmdoc-traversal-menu))
    (-> "Explain" (link tmdoc-explain-menu))
    (-> "User interface" (link tmdoc-gui-menu))
    (-> "Annotate" (link tmdoc-annotate-menu)))
  (-> "Indication" (link tmdoc-indication-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for TeXmacs documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind tmdoc-icons
  /
  (=> (balloon (icon "tm_tmdoc_title.xpm") "Enter document meta data")
      (link tmdoc-meta-menu))
  (=> (balloon (icon "tm_traverse.xpm") "Specify how to traverse the manual")
      (link tmdoc-traversal-menu))
  (=> (balloon (icon "tm_gui.xpm") "Insert user interface related markup")
      (link tmdoc-gui-menu))
  (=> (balloon (icon "tm_tmdoc_annotate.xpm") "Insert annotation")
      (link tmdoc-annotate-menu)))
