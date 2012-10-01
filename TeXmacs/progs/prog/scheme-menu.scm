;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-menu.scm
;; DESCRIPTION : Menus for scheme sessions
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

(texmacs-module (prog scheme-menu)
  (:use (prog scheme-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contextual menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (scheme-contextual-menu)
  (let* ((word (cursor-word))
         (str1 (string-append "Help with \"" word "\""))
         (str2 (string-append "Definition of \"" word "\"")))
    (if (!= word "")
        ((eval str1) (scheme-popup-help word)))
    (if (!= word "")
        ((eval str2) (scheme-go-to-definition word)))))

(menu-bind scheme-session-menu
  (link scheme-contextual-menu)
  (if (selection-active-any?)
    ("Export selection"
     (choose-file export-selected-sessions "Export sessions" "scheme")))
  ("Export scheme sessions"
    (choose-file export-sessions "Export sessions" "scheme"))
  ("Import scheme sessions"
    (choose-file import-sessions "Import sessions" "scheme"))
  ("(Re)Build autocompletion index"
   (scheme-completions-rebuild)))

; Simpler popup menu:
(menu-bind texmacs-popup-menu
  (:require (and developer-mode-on (in-prog-scheme?)))
  ("Copy verbatim" (clipboard-copy-export "verbatim" "primary"))
  ("Paste verbatim" (clipboard-paste-import "verbatim" "primary"))
  (-> "Scheme" (link scheme-session-menu))
  ---
  (-> "File" (link file-menu))
  (-> "Edit" (link edit-menu))
  (-> "View" (link view-menu))
  (-> "Go" (link go-menu))
  (if (detailed-menus?) (-> "Tools" (link tools-menu)))
  (if (with-remote-connections?) (-> "Remote" (link remote-menu)))
  (if (with-debugging-tool?) (-> "Debug" (link debug-menu)))
  (if (nnull? (test-menu)) (-> "Test" (link test-menu)))
  ---
  (-> "Help" (link help-menu)))
