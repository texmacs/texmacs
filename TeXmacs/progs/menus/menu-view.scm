
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-view.scm
;; DESCRIPTION : the view menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-view)
  (:use
    (texmacs texmacs tm-view) (texmacs texmacs tm-server)
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
	("Context dependent icons" (toggle-visible-icon-bar 1))
	("User provided icons" (toggle-visible-icon-bar 2)))
  ("Status bar" (toggle-visible-footer))
  ---
  ("Presentation mode" (toggle-full-screen-mode))
  (-> "Shrinking factor"
      ("1" (set-shrinking-factor 1))
      ("2" (set-shrinking-factor 2))
      ("3" (set-shrinking-factor 3))
      ("4" (set-shrinking-factor 4))
      ("5" (set-shrinking-factor 5))
      ("7" (set-shrinking-factor 7))
      ("10" (set-shrinking-factor 10))
      ---
      ("Other" ...
       (interactive '("Shrinking factor:") 'other-shrinking-factor))))
