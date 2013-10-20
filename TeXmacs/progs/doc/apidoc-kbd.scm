
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc-kbd.scm
;; DESCRIPTION : Keyboard shortcuts related to the API doc. system
;; COPYRIGHT   : (C) 2013 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-kbd)
  (:use (doc apidoc-widgets)))

(tm-define (macro-popup-help)
  (:synopsis "Pops up the help window for the innermost TeXmacs macro")
  (with t (tree-up (cursor-tree))
    (help-window "macros" (symbol->string (tree-label t)))))

(kbd-map
 (:require (and developer-mode? (not (in-prog-scheme?))))
 ("A-F1" (macro-popup-help)))

