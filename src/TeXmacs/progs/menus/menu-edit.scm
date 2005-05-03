
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-edit.scm
;; DESCRIPTION : the edit menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-edit)
  (:use (utils edit selections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extern-clipboard-item fm name action)
  (with routine (string->symbol (string-append "clipboard-" action))
    `(,name (,routine ,fm "primary"))))

(define-macro (extern-clipboard-menu-promise action)
  (define (item fm name) (extern-clipboard-item fm name action))
  (with routine (if (== action "paste-import")
		    converter-to-menu converter-from-menu)
    `(menu-dynamic ,@(routine "texmacs-snippet" "-snippet" #t item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Edit menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edit-menu
  ("Undo" (undo))
  ("Redo" (redo))
  ---
  (when (selection-active-any?)
	("Copy" (clipboard-copy "primary"))
	("Cut" (clipboard-cut "primary")))
  ("Paste" (clipboard-paste "primary"))
  ("Clear" (clipboard-clear "primary"))
  ---
  ("Search" ... (search-start #t))
  ("Replace" ...
   (interactive '("Replace:" "Replace by:") 'replace-start-forward))
  ("Spell" ... (spell-start))
  ---
  (when (selection-active-any?)
	(-> "Copy to"
	    (promise (extern-clipboard-menu-promise "copy-export"))
	    ---
	    ("Primary" (clipboard-copy "primary"))
	    ("Secondary" (clipboard-copy "secondary"))
	    ("Ternary" (clipboard-copy "ternary"))
	    ("Search" (clipboard-copy "search"))
	    ("Replace" (clipboard-copy "replace"))
	    ---
	    ("Other" ... (interactive '("Copy to:") 'clipboard-copy)))
	(-> "Cut to"
	    (promise (extern-clipboard-menu-promise "cut-export"))
	    ---
	    ("Primary" (clipboard-cut "primary"))
	    ("Secondary" (clipboard-cut "secondary"))
	    ("Ternary" (clipboard-cut "ternary"))
	    ("Search" (clipboard-cut "search"))
	    ("Replace" (clipboard-cut "replace"))
	    ---
	    ("Other" ... (interactive '("Cut to:") 'clipboard-cut))))
  (-> "Paste from"
      (promise (extern-clipboard-menu-promise "paste-import"))
      ---
      ("Primary" (clipboard-paste "primary"))
      ("Secondary" (clipboard-paste "secondary"))
      ("Ternary" (clipboard-paste "ternary"))
      ("Search" (clipboard-paste "search"))
      ("Replace" (clipboard-paste "replace"))
      ---
      ("Other" ... (interactive '("Paste from:") 'clipboard-paste)))
  ---
  (-> "Preferences"
      (link preferences-menu)))
