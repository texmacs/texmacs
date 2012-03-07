
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-menu.scm
;; DESCRIPTION : menus for spreadsheets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-menu)
  (:use (dynamic calc-table)))

(menu-bind calc-table-menu
  ("Textual spreadsheet" (make-calc-table 'textual-table))
  ("Numeric spreadsheet" (make-calc-table 'numeric-dot-table)))

(menu-bind calc-insert-menu
  ("Input field" (make-calc-inert))
  ("Executable input field" (make-calc-input))
  ("Field reference" (make 'calc-ref)))
