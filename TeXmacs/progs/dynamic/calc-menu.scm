
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
  ("Evaluable field" (make-calc-input))
  ("Field reference" (make 'calc-ref)))

(tm-define (pure-alternate-context? t)
  (:require (tree-in? t '(calc-inert calc-input calc-output)))
  #f)

(tm-define (hidden-child? t i)
  (:require (tree-in? t '(calc-inert calc-input calc-output)))
  (or (== i 0) (and (== i 1) (tree-atomic? (tree-ref t 1)))))
