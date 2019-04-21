
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-menu.scm
;; DESCRIPTION : menu for the Git tools
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils git git-menu)
  (:use (version version-git)
        (utils git git-tmfs)))

(menu-bind git-menu
  ;; ("Log" (git-show-log))
  ;; ("Status" (git-show-status))
  ;; ("Commit" (git-interactive-commit))
  ---
  (when (buffer-to-add? (current-buffer))
            ("Add" (git-add (current-buffer))))
  (when (buffer-to-unadd? (current-buffer))
            ("Undo Add" (git-unadd (current-buffer)))))

