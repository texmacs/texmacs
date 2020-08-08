
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cpp-edit.scm
;; DESCRIPTION : editing C++ programs
;; COPYRIGHT   :
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;
;; TO-DO: this module should provide automatic indentation and other facilities
;;        for C++ source code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog cpp-edit)
  (:use (prog prog-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cpp-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (cpp-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

; TODO: select strings first
(tm-define (kbd-select-enlarge)
  (:require prog-select-brackets?)           
  (:mode in-prog-cpp?)
  (program-select-enlarge "{" "}"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-cpp?)
  (select-brackets-after-movement "([{" ")]}" "\\"))
