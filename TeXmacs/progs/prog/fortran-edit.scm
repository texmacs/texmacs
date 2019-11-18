
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fortran-edit.scm
;; DESCRIPTION : editing Fortran 2008 programs
;; COPYRIGHT   :
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;
;; TO-DO: this module should provide automatic indentation and other facilities
;;        for Fortran source code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog fortran-edit)
  (:use (prog prog-edit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("editor:verbatim:tabstop" 4 (lambda (pref val) (noop))))

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-fortran?)
  (if (<= row 0) (get-tabstop)
      (let* ((r (program-row (- row 1)))
             (i (string-get-indent r))
             (tr (tm-string-trim r)))

         (if (or (string-prefix? "function" tr)
                 (string-prefix? "program" tr)
                 (string-prefix? "subroutine" tr)
                 (string-prefix? "interface" tr)
                 (string-prefix? "associate" tr)
                 (string-prefix? "do " tr)
                 (string-prefix? "for " tr)
                 (string-prefix? "contains" tr)
                 (string-prefix? "module" tr))
           (+ i (get-tabstop)) i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fortran-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (fortran-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-fortran?)
  (select-brackets-after-movement "([" ")]" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-fortran-pref var val)
   (syntax-read-preferences "fortran"))

(define-preferences
  ("syntax:fortran:none" "black" notify-fortran-pref)
  ("syntax:fortran:comment" "dark grey" notify-fortran-pref)
  ("syntax:fortran:keyword" "dark magenta" notify-fortran-pref)
  ("syntax:fortran:keyword_conditional" "dark magenta" notify-fortran-pref)
  ("syntax:fortran:keyword_control" "dark magenta" notify-fortran-pref)
  ("syntax:fortran:error" "dark red" notify-fortran-pref)
  ("syntax:fortran:operator" "dark red" notify-fortran-pref)
  ("syntax:fortran:operator_special" "dark red" notify-fortran-pref)
  ("syntax:fortran:operator_openclose" "dark red" notify-fortran-pref)
  ("syntax:fortran:operator_field" "dark red" notify-fortran-pref)
  ("syntax:fortran:preprocessor" "dark green" notify-fortran-pref)
  ("syntax:fortran:preprocessor_directive" "dark brown" notify-fortran-pref)
  ("syntax:fortran:declare_type" "#4040c0" notify-fortran-pref)
  ("syntax:fortran:declare_function" "#4040c0" notify-fortran-pref)
  ("syntax:fortran:variable_function" "#0000c0" notify-fortran-pref)
  ("syntax:fortran:variable_type" "dark red" notify-fortran-pref)
  ("syntax:fortran:constant" "#4040c0" notify-fortran-pref)
  ("syntax:fortran:constant_function" "#0000c0" notify-fortran-pref)
  ("syntax:fortran:constant_type" "#4040c0" notify-fortran-pref)
  ("syntax:fortran:constant_number" "#4040c0" notify-fortran-pref)
  ("syntax:fortran:constant_string" "dark red" notify-fortran-pref))
