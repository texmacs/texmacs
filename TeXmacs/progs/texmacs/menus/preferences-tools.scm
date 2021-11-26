
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

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "general preferences"))
  (division "title"
    (text "General preferences"))
  (centered
    (dynamic (general-preferences-widget))
    ======))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "keyboard preferences"))
  (division "title"
    (text "Keyboard preferences"))
  (centered
    (dynamic (keyboard-preferences-widget))
    ======))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "math preferences"))
  (division "title"
    (text "Mathematical preferences"))
  (centered
    (dynamic (math-preferences-widget*))
    ======))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "html preferences"))
  (division "title"
    (text "HTML conversion preferences"))
  (centered
    (dynamic (html-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "latex preferences"))
  (division "title"
    (text "LaTeX conversion preferences"))
  (centered
    (dynamic (latex-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "bibtex preferences"))
  (division "title"
    (text "BibTeX conversion preferences"))
  (centered
    (dynamic (bibtex-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "verbatim preferences"))
  (division "title"
    (text "Verbatim conversion preferences"))
  (centered
    (dynamic (verbatim-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "pdf preferences"))
  (division "title"
    (text "Pdf and Postscript conversion preferences"))
  (centered
    (dynamic (pdf-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "image preferences"))
  (division "title"
    (text "Image conversion preferences"))
  (centered
    (dynamic (image-preferences-widget))
    ======))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "security preferences"))
  (division "title"
    (text "Security preferences"))
  (centered
    (dynamic (security-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "miscellaneous preferences"))
  (division "title"
    (text "Miscellaneous preferences"))
  (centered
    (dynamic (misc-preferences-widget))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== tool "experimental preferences"))
  (division "title"
    (text "Experimental features (to be used with care)"))
  (centered
    (dynamic (experimental-preferences-widget*))
    ======))
