
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : theme-menu.scm
;; DESCRIPTION : menus for standard themes
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various theme-menu)
  (:use (various theme-edit)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (basic-theme-name theme)
  (let* ((s1 (string-replace theme "-" " ")))
    (upcase-first s1)))

(menu-bind basic-theme-menu
  ("Plain" (select-default-basic-theme))
  ---
  (for (theme (basic-themes))
    ((check (eval (basic-theme-name theme)) "v" (has-style-package? theme))
     (add-style-package theme)))
  ---
  ((check "Alternative colors" "v" (has-style-package? "alt-colors"))
   (toggle-style-package "alt-colors"))
  ((check "Framed theorems" "v" (has-style-package? "framed-theorems"))
   (toggle-style-package "framed-theorems")))
