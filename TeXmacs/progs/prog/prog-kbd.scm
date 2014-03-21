
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
        (prog scheme-tools) (prog scheme-edit) (prog cpp-edit)))

(kbd-map
  (:mode in-prog?)
  ; rewrite some text mode shortcuts
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop)))
  ("$" (insert "$"))
  ("$ tab" (make 'math))
  ("\\ tab" (make 'hybrid)))

(kbd-map
  (:mode in-prog-scheme?)
  ("cmd i" (scheme-indent))
  ("cmd tab" (scheme-indent))
  ("cmd A-tab" (scheme-program-indent))
  ("std c" (clipboard-copy-export "scheme" "primary"))
  ("std v" (clipboard-paste-import "scheme" "primary"))
  ("std x" (clipboard-cut-export "scheme" "primary"))
  ("(" (scheme-bracket-open #\( #\) ))
  (")" (scheme-bracket-close #\( #\) ))
  ("[" (scheme-bracket-open #\[ #\] ))
  ("]" (scheme-bracket-close #\[ #\] ))
  ("\"" (scheme-bracket-open #\" #\" )))

(kbd-map
  (:require (and developer-mode? (in-prog-scheme?)))
  ("A-F1" (scheme-popup-help (cursor-word)))
  ("cmd A-F1" (scheme-inbuffer-help (cursor-word)))
  ("std F1" (scheme-go-to-definition (cursor-word))))

(kbd-map
  (:require (and developer-mode? (in-prog-scheme?) 
                 (== "scheme-file" (file-format (current-buffer-url)))))
  ("F5" (run-scheme-file (current-buffer-url))))

(kbd-map
  (:mode in-prog-cpp?)
  ("{" (cpp-bracket-open #\{ #\} ))
  ("}" (cpp-bracket-close #\{ #\} ))
  ("(" (cpp-bracket-open #\( #\) ))
  (")" (cpp-bracket-close #\( #\) ))
  ("[" (cpp-bracket-open #\[ #\] ))
  ("]" (cpp-bracket-close #\[ #\] ))
  ("\"" (cpp-bracket-open #\" #\" )))