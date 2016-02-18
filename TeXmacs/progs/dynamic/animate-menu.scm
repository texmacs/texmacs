
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : animate-menu.scm
;; DESCRIPTION : menus for editing animations
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic animate-menu)
  (:use (dynamic animate-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (anim-input-icon name t i setter)
  (with in (tree->string (tree-ref t i))
    (mini #t
      (group (eval (string-append name ":")))
      (input (setter answer) "string" (list in) "5em"))))

(tm-menu (anim-duration-icon name t i)
  (with setter (lambda (x) (when x (tree-set t i x)))
    (dynamic (anim-input-icon name t i setter))))

(tm-menu (anim-now-icon name t i)
  (with setter (lambda (x) (when x (anim-set-now t x)))
    (dynamic (anim-input-icon name t i setter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized focus icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-hidden-icons t)
  (:require (tree-is? t 'anim-edit)))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(anim-static anim-dynamic)))
  //
  (dynamic (anim-duration-icon "Duration" t 1))
  (dynamic (anim-duration-icon "Step" t 2))
  (dynamic (anim-duration-icon "Now" t 3))
  //
  ("Edit" (anim-checkout t)))

(tm-menu (animate-focus-icons t)
  //
  (dynamic (anim-duration-icon "Duration" t 2))
  (dynamic (anim-duration-icon "Step" t 3))
  (dynamic (anim-now-icon "Now" t 4))
  //
  ("Play" (anim-commit t)))
