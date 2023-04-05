
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-tools.scm
;; DESCRIPTION : the preferences widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus preferences-tools)
  (:use (texmacs menus preferences-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (general-preferences-tool win)
  (:name "General preferences")
  (dynamic (general-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (keyboard-preferences-tool win)
  (:name "Keyboard preferences")
  (dynamic (keyboard-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (math-preferences-tool win)
  (:name "Mathematical preferences")
  (dynamic (math-preferences-widget*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (html-preferences-tool win)
  (:name "HTML conversion preferences")
  (dynamic (html-preferences-widget)))

(tm-tool (latex-preferences-tool win)
  (:name "LaTeX conversion preferences")
  (dynamic (latex-preferences-widget)))

(tm-tool (bibtex-preferences-tool win)
  (:name "BibTeX conversion preferences")
  (dynamic (bibtex-preferences-widget)))

(tm-tool (verbatim-preferences-tool win)
  (:name "Verbatim conversion preferences")
  (dynamic (verbatim-preferences-widget)))

(tm-tool (pdf-preferences-tool win)
  (:name "Pdf and Postscript conversion preferences")
  (dynamic (pdf-preferences-widget)))

(tm-tool (image-preferences-tool win)
  (:name "Image conversion preferences")
  (dynamic (image-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (security-preferences-tool win)
  (:name "Security preferences")
  (dynamic (security-preferences-widget)))

(tm-tool (misc-preferences-tool win)
  (:name "Miscellaneous preferences")
  (dynamic (misc-preferences-widget)))

(tm-tool (experimental-preferences-tool win)
  (:name "Experimental features (to be used with care)")
  (dynamic (experimental-preferences-widget*)))
