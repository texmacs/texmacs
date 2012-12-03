
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
  ("Side tools" (toggle-visible-side-tools 0))
  ("Status bar" (toggle-visible-footer))
  ---
  ("Full screen mode"  (toggle-full-screen-edit-mode))
  ("Presentation mode" (toggle-full-screen-mode))
  ("Remote control" (toggle-remote-control-mode))
  ---
  (-> "Zoom"
      ("500%" (change-zoom-factor (/ 5.0 1.0)))
      ("250%" (change-zoom-factor (/ 5.0 2.0)))
      ("167%" (change-zoom-factor (/ 5.0 3.0)))
      ("125%" (change-zoom-factor (/ 5.0 4.0)))
      ("100%" (change-zoom-factor 1.0))
      ("71%" (change-zoom-factor (/ 5.0 7.0)))
      ("50%" (change-zoom-factor (/ 5.0 10.0)))
      ---
      ("Other" (interactive other-zoom-factor))))
