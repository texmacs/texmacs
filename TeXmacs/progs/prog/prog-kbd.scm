
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-kbd.scm
;; DESCRIPTION : Shortcuts for program modes
;; COPYRIGHT   : (C) 2013 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-kbd)
  (:use (kernel gui kbd-define)
        (utils edit selections)
        (prog scheme-tools) (prog scheme-edit)))

(kbd-map
  (:require (in-prog-scheme?))
  ("C-i" (scheme-indent))
  ("C-tab" (scheme-indent))
  ("std c" (clipboard-copy-export "scheme" "primary"))
  ("std v" (clipboard-paste-import "scheme" "primary"))
  ("std x" (clipboard-cut-export "scheme" "primary")))

(kbd-map
  (:require (and developer-mode? (in-prog-scheme?)))
  ("A-F1" (scheme-popup-help (cursor-word)))
  ("S-A-F1" (scheme-inbuffer-help (cursor-word)))
  ("M-F1" (scheme-go-to-definition (cursor-word)))
  ("F5" (run-scheme-file (current-buffer-url))))

(kbd-map ; rewrite some text mode shortcuts
  (:mode in-prog?)
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop))))
