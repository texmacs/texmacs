
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: fcf788f7-bf8b-4abb-93cf-102ab6ffd8fc
;;
;; MODULE      : kbd-proclus.scm
;; DESCRIPTION : Keyboard bindings for the 'proclus' plugin
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

(texmacs-module (keyboard kbd-proclus)
  (:use (proclus)
        (proclus-distill)
        (proclus-source)))

;; Shortcuts are enabled in-proclus? even if they are only relevant in
;; sub-modes so they will appear in disabled menu items.

(kbd-map in-proclus?
  ;; in-proclus-editable?
  ("proclus s" (active-source))
  ("proclus b" (active-but))
  ("proclus <" (locus))
  ("proclus 1" (go-to-last-locus)))

(kbd-map in-proclus-links?
  ("proclus S" (go-to-source-buffer))) ;; FIXME: unify with go-to-source
