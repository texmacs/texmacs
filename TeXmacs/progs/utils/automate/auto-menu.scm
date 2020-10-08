
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-menu.scm
;; DESCRIPTION : Menus for automated document generation
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils automate auto-menu)
  (:use (utils automate auto-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main menu for automated document generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind automate-menu
  (-> "Block"
      ("if" (make-block-if))
      ("if-else" (make-block-if-else))
      ("for" (make-block-for))
      ("while" (make-block-while))
      ---
      ("assign" (make-block-assign))
      ---
      ("intersperse" (make-block-intersperse))
      ("tag" (make-block-tag)))
  (-> "Inline"
      ("if" (make-inline-if))
      ("if-else" (make-inline-if-else))
      ("for" (make-inline-for))
      ("while" (make-inline-while))
      ---
      ("assign" (make-inline-assign))
      ---
      ("intersperse" (make-inline-intersperse))
      ("tag" (make-inline-tag)))
  (-> "Output"
      ("String" (make-output-string))
      ("Inline content" (make-inline-output))
      ("Block content" (make-block-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert menu as extra top level menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-menu
  (former)
  (if (style-has? "automate-dtd")
      (=> "Automate"
	  (link automate-menu))))
