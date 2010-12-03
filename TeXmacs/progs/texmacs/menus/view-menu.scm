
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : view-menu.scm
;; DESCRIPTION : the view menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus view-menu)
  (:use
    (texmacs texmacs tm-view)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The View menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind view-menu
  ("Open new window" (open-window))
  ("Clone window" (clone-window))
  ("Close window" (safely-kill-window))
  ---
  ("Header" (toggle-visible-header))
  (when (visible-header?)
	("Main icon bar" (toggle-visible-icon-bar 0))
	("Mode dependent icons" (toggle-visible-icon-bar 1))
	("Focus dependent icons" (toggle-visible-icon-bar 2))
	("User provided icons" (toggle-visible-icon-bar 3)))
  ("Status bar" (toggle-visible-footer))
  ---
  ("Full screen mode"  (toggle-full-screen-edit-mode))
  ("Presentation mode" (toggle-full-screen-mode))
  ("Remote control" (toggle-remote-control-mode))
  ---
  (-> "Shrinking factor"
      ("1" (set-shrinking-factor 1))
      ("2" (set-shrinking-factor 2))
      ("3" (set-shrinking-factor 3))
      ("4" (set-shrinking-factor 4))
      ("5" (set-shrinking-factor 5))
      ("7" (set-shrinking-factor 7))
      ("10" (set-shrinking-factor 10))
      ---
      ("Other" (interactive other-shrinking-factor))))
