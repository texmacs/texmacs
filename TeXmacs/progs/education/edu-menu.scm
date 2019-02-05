
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-menu.scm
;; DESCRIPTION : menus for educational purposes
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-menu)
  (:use (education edu-edit)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Education menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind edu-menu
  ("Problem mode" (edu-set-mode :problem))
  ("Solution mode" (edu-set-mode :solution))
  ;;("Training mode" (edu-set-mode :training))
  )

(menu-bind course-menu
  (link edu-menu))

(menu-bind exam-menu
  (link edu-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-edu-options)
  (if (has-style-package? "edu-compact") "Compact" "Regular"))

(menu-bind edu-style-options-menu
  ("Regular" (remove-style-package* "edu-compact"))
  ("Compact" (add-style-package "edu-compact"))
  ---
  (link page-layout-menu))

(tm-menu (focus-style-extra-menu t)
  (:require (and (style-has? "std-edu-dtd")
                 (not (style-has? "beamer-style"))))
  (=> (eval (current-edu-options))
      (link edu-style-options-menu)))

(tm-menu (focus-style-extra-icons t)
  (:require (and (style-has? "std-edu-dtd")
                 (not (style-has? "beamer-style"))))
  (=> (balloon (eval (current-edu-options)) "Style options")
      (link edu-style-options-menu)))
