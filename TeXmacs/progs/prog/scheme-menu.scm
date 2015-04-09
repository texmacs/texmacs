;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-menu.scm
;; DESCRIPTION : Menus for scheme files and sessions
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
         (str1 (replace "Help with \"%1\"" word))
         (str2 (replace "Definition of \"%1\"" word)))
    (if (!= word "")
        ((eval str1) (scheme-popup-help word)))
    (if (!= word "")
        ((eval str2) (scheme-go-to-definition word)))))

(menu-bind scheme-menu
  (if (in-prog-scheme?)
      (link scheme-contextual-menu))
  (if (and (in-prog-scheme?) (selection-active-any?))
      ("Export selection"
       (choose-file export-selected-sessions "Export sessions" "scheme")))
  (when (and (in-prog-scheme?) (in-session?))
    ("Export sessions"
     (choose-file export-sessions "Export sessions" "scheme")))
  ("Import sessions"
   (choose-file import-sessions "Import sessions" "scheme"))
  ("(Re)Build autocompletion index"
   (scheme-completions-rebuild))
   (if (and (in-prog-scheme?)
            (== "scheme-file" (file-format (current-buffer-url))))
       ("Run current file" (run-scheme-file (current-buffer-url)))))

; Simpler popup menu.
(menu-bind texmacs-alternative-popup-menu
  (:require (in-prog-scheme?))
  (-> "File" (link file-menu))
  (-> "Edit" (link edit-menu))
  (-> "View" (link view-menu))
  (-> "Go" (link go-menu))
  (if (detailed-menus?) (-> "Tools" (link tools-menu)))
  (if (with-remote-tool?) (-> "Remote" (link remote-menu)))
  (if (with-debugging-tool?) (-> "Debug" (link debug-menu)))
  (if (nnull? (test-menu)) (-> "Test" (link test-menu)))
  (-> "Scheme" (link scheme-menu))
  ---
  (-> "Help" (link help-menu)))
