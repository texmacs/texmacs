
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : python-edit.scm
;; DESCRIPTION : Editing python programs
;; COPYRIGHT   : (C) 2014
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog python-edit)
  (:use (prog prog-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-python?)
  (if (<= row 0) 0
      (let* ((s (program-row (- row 1)))
             (i (string-get-indent s))
             (c (if (== s "") "" (string-take-right s 1))))
        (if (== c ":") (+ i (get-tabstop)) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (python-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (python-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

; TODO: select strings first
;(tm-define (kbd-select-enlarge)
;  (:require prog-select-brackets?)
;  (:mode in-prog-python?)
;  (program-select-enlarge "(" ")"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-python?)
  (select-brackets-after-movement "([{" ")]}" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-python-syntax var val)
  (syntax-read-preferences "python"))

(define-preferences
  ("syntax:python:none" "red" notify-python-syntax)
  ("syntax:python:comment" "brown" notify-python-syntax)
  ("syntax:python:error" "dark red" notify-python-syntax)
  ("syntax:python:constant" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_identifier" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_function" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_type" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_category" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_module" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_number" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_string" "dark grey" notify-python-syntax)
  ("syntax:python:constant_char" "#333333" notify-python-syntax)
  ("syntax:python:variable" "#606060" notify-python-syntax)
  ("syntax:python:variable_identifier" "#204080" notify-python-syntax)
  ("syntax:python:variable_function" "#606060" notify-python-syntax)
  ("syntax:python:variable_type" "#00c000" notify-python-syntax)
  ("syntax:python:variable_category" "#00c000" notify-python-syntax)
  ("syntax:python:variable_module" "#00c000" notify-python-syntax)
  ("syntax:python:variable_ioarg" "#00b000" notify-python-syntax)
  ("syntax:python:declare" "#0000c0" notify-python-syntax)
  ("syntax:python:declare_identifier" "#0000c0" notify-python-syntax)
  ("syntax:python:declare_function" "#0000c0" notify-python-syntax)
  ("syntax:python:declare_type" "#0000c0" notify-python-syntax)
  ("syntax:python:declare_category" "#d030d0" notify-python-syntax)
  ("syntax:python:declare_module" "#0000c0" notify-python-syntax)
  ("syntax:python:operator" "#8b008b" notify-python-syntax)
  ("syntax:python:operator_openclose" "#B02020" notify-python-syntax)
  ("syntax:python:operator_field" "#88888" notify-python-syntax)
  ("syntax:python:operator_special" "orange" notify-python-syntax)
  ("syntax:python:operator" "#0000c0" notify-python-syntax)
  ("syntax:python:keyword" "#309090" notify-python-syntax)
  ("syntax:python:keyword_conditional" "#309090" notify-python-syntax)
  ("syntax:python:keyword_control" "#309090" notify-python-syntax))

