
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : embedded-menu.scm
;; DESCRIPTION : menus for setting local formatting properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic embedded-menu)
  (:use (generic generic-menu)
	(generic embedded-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special menus for images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-misc-menu t)
  (:require (embedded-image-context? t))
  (-> "Save image"
      ("Save image as" (save-embedded-image-as))
      ("Link image as" (link-embedded-image-as))
      ("Link image and copies as" (link-embedded-image-copies-as))
      ---
      ("Save all embedded images" (save-all-embedded-images))
      ("Link all embedded images" (link-all-embedded-images))))

(tm-menu (focus-misc-menu t)
  (:require (linked-image-context? t))
  (-> "Embed image"
      ("Embed this image" (embed-this-image))
      ---
      ("Embed all linked images" (embed-all-images))))
