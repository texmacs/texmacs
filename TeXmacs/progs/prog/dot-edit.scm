
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : dot-edit.scm
;; DESCRIPTION : editing DOT programs
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog dot-edit)
  (:use (prog prog-edit)))

(tm-define (get-tabstop)
  (:mode in-prog-dot?)
  2)

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-dot?)
  (get-tabstop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (dot-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (dot-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-dot?)
  (select-brackets-after-movement "([{" ")]}" "\\"))

(kbd-map
  (:mode in-prog-dot?)
  ("A-tab" (insert-tabstop))
  ("cmd S-tab" (remove-tabstop)) ; TEMP (see above)
  ("{" (dot-bracket-open "{" "}" ))
  ("}" (dot-bracket-close "{" "}" ))
  ("(" (dot-bracket-open "(" ")" ))
  (")" (dot-bracket-close "(" ")" ))
  ("[" (dot-bracket-open "[" "]" ))
  ("]" (dot-bracket-close "[" "]" ))
  ("\"" (dot-bracket-open "\"" "\"" ))
  ("'" (dot-bracket-open "'" "'" )))
