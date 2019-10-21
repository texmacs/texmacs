;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc-menu.scm
;; DESCRIPTION : Menu items for the API doc. system
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The contents of this file are preliminary and simple. Things TO-DO are:
;;  - this list 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-menu)
  (:use (prog scheme-tools)
        (doc apidoc-widgets)))

(menu-bind apidoc-manual-menu
  ("Insert symbol documentation" (interactive ask-insert-symbol-doc))
  ("Open module browser" (open-module-browser)))

(menu-bind apidoc-menu
  ("Delete documentation cache" (doc-delete-cache))
  ("Browse modules documentation" (apidoc-all-modules))
  ("Browse symbols documentation" (apidoc-all-symbols))
  ("Open module browser" (open-module-browser))
  ("Open symbol browser" (open-symbol-browser))
  (if (in-tmdoc?)
      ("Insert symbol documentation" (interactive ask-insert-symbol-doc))))
