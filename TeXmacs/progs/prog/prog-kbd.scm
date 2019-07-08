
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
        (prog scheme-tools) 
        (prog scheme-edit)
        (prog scala-edit)
        (prog cpp-edit)
        (prog python-edit)
        (prog fortran-edit)))

(kbd-map
  (:mode in-prog?)
  ("cmd i" (program-indent #f))
  ("cmd I" (program-indent #t)) ; TODO
  ("cmd tab" (program-indent #f))
  ("cmd S-tab" (program-indent #t)) ; TODO
  ("cmd A-tab" (program-indent-all #f))

  ;; override some text mode shortcuts
  ("space var" (insert-tabstop))
  ("space var var" (begin (insert-tabstop) (insert-tabstop)))
  ("$" (insert "$"))
  ("$ var" (make 'math))
  ("\\" "\\")
  ("\\ var" (make 'hybrid))
  ("\"" "\"")
  ("`" "`")
  ("` var" "<#2018>")
  ("'" "'")
  ("' var" "<#2019>")
  ("< <" "<less><less>")
  ("> >" "<gtr><gtr>")
  ("' '" "''")
  ("` `" "``")
  ("- -" "--")
  ("- - -" "---")
  ("< < var" "")
  ("> > var" "")
  ("' ' var" "")
  ("` ` var" "")
  (", , var" "")
  ("- - var" "")
  ("- - - var" ""))

(kbd-map
  (:mode in-prog-scheme?)
  ("std c" (clipboard-copy-export "scheme" "primary"))
  ("std v" (clipboard-paste-import "scheme" "primary"))
  ("std x" (clipboard-cut-export "scheme" "primary"))
  ("(" (scheme-bracket-open "(" ")" ))
  (")" (scheme-bracket-close "(" ")" ))
  ("[" (scheme-bracket-open "[" "]" ))
  ("]" (scheme-bracket-close "[" "]" ))
  ("\"" (scheme-bracket-open "\"" "\"")))

(kbd-map
  (:require (and developer-mode? (in-prog-scheme?)))
  ("A-F1" (scheme-popup-help (cursor-word)))
  ("cmd A-F1" (scheme-inbuffer-help (cursor-word)))
  ("std F1" (scheme-go-to-definition (cursor-word))))

(kbd-map
  (:require (and developer-mode? (in-prog-scheme?) 
                 (== "scheme-file" (file-format (current-buffer-url)))))
  ("std R" (run-scheme-file (current-buffer-url))))

(kbd-map
  (:mode in-prog-cpp?)
  ("{" (cpp-bracket-open "{" "}" ))
  ("}" (cpp-bracket-close "{" "}" ))
  ("(" (cpp-bracket-open "(" ")" ))
  (")" (cpp-bracket-close "(" ")" ))
  ("[" (cpp-bracket-open "[" "]" ))
  ("]" (cpp-bracket-close "[" "]" ))
  ("\"" (cpp-bracket-open "\"" "\"" )))

;HACK: should rewrite program-indent-line to accept unindent
(define (remove-tabstop)
  (with w (get-tabstop)
    (with c (program-get-indent)
      (if (>= c w) (program-set-indent (- c w))))))

(kbd-map
  (:mode in-prog-python?)
  ("A-tab" (insert-tabstop))
  ("cmd S-tab" (remove-tabstop)) ; TEMP (see above)
  ("{" (python-bracket-open "{" "}" ))
  ("}" (python-bracket-close "{" "}" ))
  ("(" (python-bracket-open "(" ")" ))
  (")" (python-bracket-close "(" ")" ))
  ("[" (python-bracket-open "[" "]" ))
  ("]" (python-bracket-close "[" "]" ))
  ("\"" (python-bracket-open "\"" "\"" ))
  ("'" (python-bracket-open "'" "'" )))

(kbd-map
  (:mode in-prog-fortran?)
  ("(" (fortran-bracket-open "(" ")" ))
  (")" (fortran-bracket-close "(" ")" ))
  ("[" (fortran-bracket-open "[" "]" ))
  ("]" (fortran-bracket-close "[" "]" ))
  ("\"" (fortran-bracket-open "\"" "\"" ))
  ("'" (fortran-bracket-open "'" "'" )))
