
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for spread sheets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-kbd)
  (:use (math math-kbd)    
        (dynamic calc-table)))

(kbd-map
  (:require (inside? 'calc-table))
  (", ," (make 'cell-commas))
  ("+ +" (make 'cell-plusses)))

(kbd-map
  (:require (calc-ready?))
  ("\\ \\" (make-calc-inert))
  ("\\ !" (make-calc-input))
  ("\\ ?" (make 'calc-ref)))
