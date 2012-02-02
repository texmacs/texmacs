
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
  ("Title" (make 'tmdoc-title))
  ("Copyright" (make 'tmdoc-copyright))
  ("License" (make 'tmdoc-license)))

(menu-bind tmdoc-traversal-menu
  ("Traverse" (make 'traverse))
  ("Branch" (make 'branch))
  ("Extra branch" (make 'extra-branch))
  ("Continue" (make 'continue)))

(menu-bind tmdoc-gui-menu
  ("Keyboard shortcut" (make 'shortcut))
  ("Explicit keystroke" (make 'key))
  ---
  ("Menu" (make 'menu))
  ("Submenu" (make 'submenu))
  ("Subsubmenu" (make 'subsubmenu))
  ("Subsubsubmenu" (make 'subsubsubmenu)))

(menu-bind tmdoc-annotate-menu
  ("TeXmacs tag" (make 'markup))
  ("TeXmacs style" (make 'tmstyle))
  ("TeXmacs package" (make 'tmpackage))
  ("TeXmacs DTD" (make 'tmdtd)))

(menu-bind tmdoc-menu
  (-> "Meta data" (link tmdoc-meta-menu))
  (-> "Traversal" (link tmdoc-traversal-menu))
  (-> "User interface" (link tmdoc-gui-menu))
  (-> "Annotate" (link tmdoc-annotate-menu)))
