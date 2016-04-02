
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
  ("Full screen mode"  (toggle-full-screen-edit-mode))
  ("Presentation mode" (toggle-full-screen-mode))
  ("Show panorama" (toggle-panorama-mode))
  ("Remote control" (toggle-remote-control-mode))
  ---
  ("Fit to screen" (fit-to-screen))
  ("Fit to screen width" (fit-to-screen-width))
  ;;("Fit to screen height" (fit-to-screen-height))
  ("Zoom in" (zoom-in (sqrt (sqrt 2.0))))
  ("Zoom out" (zoom-out (sqrt (sqrt 2.0))))
  (-> "Zoom"
      ("50%"  (change-zoom-factor 0.5))
      ("71%"  (change-zoom-factor (sqrt 0.5)))
      ("100%" (change-zoom-factor 1.0))
      ("141%" (change-zoom-factor (sqrt 2.0)))
      ("200%" (change-zoom-factor 2.0))
      ---
      ("Other" (interactive other-zoom-factor)))
  ---
  ("Header" (toggle-visible-header))
  (when (visible-header?)
        ("Main icon bar" (toggle-visible-icon-bar 0))
        ("Mode dependent icons" (toggle-visible-icon-bar 1))
        ("Focus dependent icons" (toggle-visible-icon-bar 2))
        ("User provided icons" (toggle-visible-icon-bar 3)))
  ("Status bar" (toggle-visible-footer))
  (if #f ; Side tools are for now (v1.99.2) disabled.
      ("Side tools" (toggle-visible-side-tools 0)))
  (if #f ; Search bar cannot be toggled from here, remove the menu item?
      ("Bottom tools" (toggle-visible-bottom-tools 0))))
