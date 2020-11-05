
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ref-menu.scm
;; DESCRIPTION : extra menus for reference management
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link ref-menu)
  (:use (link ref-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind same-tie-menu
  (when (same-ties)
    (group (eval (tie-id)))
    ("First occurrence" (special-first))
    ("Previous occurrence" (special-previous))
    ("Next occurrence" (special-next))
    ("Last occurrence" (special-last))))

(menu-bind duplicate-labels-menu
  (when (duplicate-labels)
    (group "Duplicate labels")
    ("First error" (special-return))
    ("Previous error" (special-back))
    ("Next error" (special-forward))
    ("Last error" (special-shift-return))))

(menu-bind broken-references-menu
  (when (broken-references)
    (group "Broken references")
    ("First error" (special-return))
    ("Previous error" (special-back))
    ("Next error" (special-forward))
    ("Last error" (special-shift-return))))

(menu-bind broken-citations-menu
  (when (broken-citations)
    (group "Broken citations")
    ("First error" (special-return))
    ("Previous error" (special-back))
    ("Next error" (special-forward))
    ("Last error" (special-shift-return))))

(tm-define (focus-has-search-menu? t)
  (:require (and (tie-context? t) (not (cursor-inside? t))))
  #t)

(tm-menu (focus-search-menu t)
  (:require (label-context? t))
  (link same-tie-menu)
  ---
  (link duplicate-labels-menu))

(tm-menu (focus-search-menu t)
  (:require (reference-context? t))
  (link same-tie-menu)
  ---
  (link broken-references-menu))

(tm-menu (focus-search-menu t)
  (:require (citation-context? t))
  (link same-tie-menu)
  ---
  (link broken-citations-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main menus for reference management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind ref-menu
  ("Fix duplicate labels" (go-to-duplicate-label :first))
  ("Fix broken references" (go-to-broken-reference :first))
  ("Fix broken citations" (go-to-broken-citation :first)))
