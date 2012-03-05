
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-drd.scm
;; DESCRIPTION : data relation definitions for spreadsheet related tags
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-drd)
  (:use (dynamic dynamic-drd)))

(define-toggle calc-input calc-output)
(define-toggle cell-input cell-output)

(tm-define (calc-data-context? t)
  (tree-in? t '(calc-inert calc-input calc-output
                cell-inert cell-input cell-output)))

(tm-define (calc-inert-context? t)
  (tree-in? t '(calc-inert cell-inert)))

(tm-define (calc-toggle-context? t)
  (tree-in? t '(calc-input calc-output cell-input cell-output)))

(tm-define (calc-ref-context? t)
  (tree-in? t '(calc-ref cell-ref)))
