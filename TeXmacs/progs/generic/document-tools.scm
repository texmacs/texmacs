
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-tools.scm
;; DESCRIPTION : tools for setting global document properties
;; COPYRIGHT   : (C) 2023  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-tools)
  (:use (generic document-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool* (source-tree-preferences-tool win)
  (:name "Source tree preferences")
  (with u (current-buffer)
    (dynamic ((source-tree-preferences-editor u)
              (tool-quit 'source-tree-preferences-tool win)))))

(tm-tool* (document-metadata-tool win)
  (:name "Document metadata")
  (with u (current-buffer)
    (dynamic ((document-metadata-editor u)
              (tool-quit 'document-metadata-tool win)))))

(tm-tool* (document-colors-tool win)
  (:name "Document colors")
  (with u (current-buffer)
    (dynamic ((document-colors-picker u)
              (tool-quit 'document-colors-tool win)))))
