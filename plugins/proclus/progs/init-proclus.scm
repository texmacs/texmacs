
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 597691d0-01d9-41ab-bc31-2141373c702b
;;
;; MODULE      : init-proclus.scm
;; DESCRIPTION : Initialize the 'proclus' plugin
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(lazy-define (proclus-distill) locus-action)
(lazy-define (proclus-distill) loci-editor-action)
(use-modules (menu menu-proclus))

(use-modules (proclus-types)) ;; FIXME: for interactive
(use-modules (proclus-absname)) ;; FIXME: for choose-file
(use-modules (proclus)) ;; FIXME: for interactive
(use-modules (proclus-distill)) ;; FIXME: for menu

(define (proclus-initialize)
  (display "Installing Proclus.\n")
  (kbd-wildcards pre
    ("proclus" "emacs:contextual"))
  (lazy-keyboard (keyboard kbd-proclus) in-proclus?)
  (menu-extend texmacs-extra-menu
    (if (in-proclus-editable?)
        (link menu-proclus))
    (if (in-proclus-links?)
        (link menu-proclus-links))))

(plugin-configure proclus
  (:require #t)
  (:initialize (proclus-initialize)))

;; Has to be after plugin-configure to redefine the in-proclus? predicate
(texmacs-modes
  (in-proclus% (style-has? "proclus-dtd"))
  (in-proclus-locus% (inside? 'locus) in-proclus%)
  (in-proclus-editable% (style-has? "proclus-editable-dtd") in-proclus%)
  (in-proclus-links% (style-has? "proclus-links-dtd") in-proclus%))

(lazy-menu (menu menu-proclus) proclus-popup-menu)

(menu-bind texmacs-popup-menu
  (:mode in-proclus?)
  (link proclus-popup-menu))
