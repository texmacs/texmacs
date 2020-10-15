
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : theme-edit.scm
;; DESCRIPTION : basic themes
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various theme-edit)
  (:use (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style package rules for themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (basic-themes)
  (list "blackboard" "bluish" "boring-white" "dark-vador" "granite"
        "ice" "manila-paper" "metal" "pale-blue" "parchment"
        "pine" "reddish" "ridged-paper" "rough-paper"
        "xperiment"))

(tm-define (current-basic-theme)
  (with l (get-style-list)
    (or (list-find l (cut in? <> (basic-themes))) "plain")))

(tm-define (default-basic-theme?)
  (== (current-basic-theme) "plain"))

(tm-define (select-default-basic-theme)
  (:check-mark "v" default-basic-theme?)
  (with theme (current-basic-theme)
    (when (!= theme "plain")
      (toggle-style-package theme))))

(tm-define (style-category p)
  (:require (in? p (basic-themes)))
  :basic-theme)

(tm-define (style-category-precedes? x y)
  (:require (and (== x :basic-theme)
                 (in? y (list :theorem-decorations))))
  #t)
