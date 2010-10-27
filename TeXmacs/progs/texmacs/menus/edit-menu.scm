
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

(define (extern-clipboard-item fm name action)
  (with routine (string->symbol (string-append "clipboard-" action))
    `(,name (,routine ,fm "primary"))))

(define-macro (extern-clipboard-menu-promise action)
  (define (item fm name) (extern-clipboard-item fm name action))
  (with routine (if (== action "paste-import")
		    converter-to-menu converter-from-menu)
    `(menu-dynamic ,@(routine "texmacs-snippet" "-snippet" #t item))))

(define (redo-item i)
  (list `(concat "Branch " ,(number->string (+ i 1)))
	(lambda () (redo i))))

(tm-define (redo-menu)
  (menu-dynamic
    ,@(map redo-item (.. 0 (redo-possibilities)))))

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
	    (promise (extern-clipboard-menu-promise "copy-export"))
	    ---
	    ("Primary" (clipboard-copy "primary"))
	    ("Secondary" (clipboard-copy "secondary"))
	    ("Ternary" (clipboard-copy "ternary"))
	    ("Search" (clipboard-copy "search"))
	    ("Replace" (clipboard-copy "replace"))
	    ---
	    ("Other" (interactive clipboard-copy)))
	(-> "Cut to"
	    (promise (extern-clipboard-menu-promise "cut-export"))
	    ---
	    ("Primary" (clipboard-cut "primary"))
	    ("Secondary" (clipboard-cut "secondary"))
	    ("Ternary" (clipboard-cut "ternary"))
	    ("Search" (clipboard-cut "search"))
	    ("Replace" (clipboard-cut "replace"))
	    ---
	    ("Other" (interactive clipboard-cut))))
      (-> "Paste from"
	  (promise (extern-clipboard-menu-promise "paste-import"))
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
