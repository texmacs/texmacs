
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edit-menu.scm
;; DESCRIPTION : the edit menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus edit-menu)
  (:use (utils library cursor)
	(utils edit selections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (clipboard-extern-menu cvs fun)
  (with l (cvs "texmacs-snippet" "-snippet" #t)
    (for (fm l)
      (with name (format-get-name fm)
        ((eval name) (fun fm "primary"))))))

(tm-define (clipboard-copy-export-menu)
  (clipboard-extern-menu converters-from-special clipboard-copy-export))
(tm-define (clipboard-cut-export-menu)
  (clipboard-extern-menu converters-from-special clipboard-cut-export))
(tm-define (clipboard-paste-import-menu)
  (clipboard-extern-menu converters-to-special clipboard-paste-import))

(tm-menu (redo-menu)
  (for (i (.. 0 (redo-possibilities)))
    ((eval `(concat "Branch " ,(number->string (+ i 1)))) (redo i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Edit menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edit-menu
  (when (> (undo-possibilities) 0)
    ("Undo" (undo 0)))
  (when (> (redo-possibilities) 0)
    (if (<= (redo-possibilities) 1)
	("Redo" (redo 0)))
    (if (> (redo-possibilities) 1)
	(-> "Redo" (link redo-menu))))
  ---
  (when (or (selection-active-any?)
	    (and (in-graphics?)
		 (graphics-selection-active?)))
	("Copy" (clipboard-copy "primary"))
	("Cut" (clipboard-cut "primary")))
  ("Paste" (clipboard-paste "primary"))
  (if (detailed-menus?)
      ("Clear" (clipboard-clear "primary")))
  ---
  ("Search" ... (search-start #t))
  ("Replace" (interactive replace-start-forward))

  (if (not (in-math?))
      ("Spell" ... (spell-start)))
  (if (in-math?)
      (=> "Correct"
          (link math-correct-menu)))
  (if (detailed-menus?)
      ---
      (when (selection-active-any?)
	(-> "Copy to"
	    (link clipboard-copy-export-menu)
	    ---
	    ("Primary" (clipboard-copy "primary"))
	    ("Secondary" (clipboard-copy "secondary"))
	    ("Ternary" (clipboard-copy "ternary"))
	    ("Search" (clipboard-copy "search"))
	    ("Replace" (clipboard-copy "replace"))
	    ---
	    ("Other" (interactive clipboard-copy)))
	(-> "Cut to"
	    (link clipboard-cut-export-menu)
	    ---
	    ("Primary" (clipboard-cut "primary"))
	    ("Secondary" (clipboard-cut "secondary"))
	    ("Ternary" (clipboard-cut "ternary"))
	    ("Search" (clipboard-cut "search"))
	    ("Replace" (clipboard-cut "replace"))
	    ---
	    ("Other" (interactive clipboard-cut))))
      (-> "Paste from"
          (link clipboard-paste-import-menu)
	  ---
	  ("Primary" (clipboard-paste "primary"))
	  ("Secondary" (clipboard-paste "secondary"))
	  ("Ternary" (clipboard-paste "ternary"))
	  ("Search" (clipboard-paste "search"))
	  ("Replace" (clipboard-paste "replace"))
	  ---
	  ("Other" (interactive clipboard-paste))))
  ---
  (-> "Preferences"
      (link preferences-menu)))
