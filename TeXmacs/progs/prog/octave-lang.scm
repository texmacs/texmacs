
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : octave-lang.scm
;; DESCRIPTION : the Octave Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog octave-lang))

(tm-define (octave-keywords)
  (list
   `(constant
     "false" "true")
   `(declare_type "function" "endfunction" "class")
   `(declare_module "pkg")
   `(keyword_conditional
     "break" "continue" "do" "else" "for" "endfor" "if" "endif"
     "while" "endwhile" "switch" "case")
   `(keyword_control
     "catch" "try")))

(tm-define (octave-operators)
  (list
   `(operator
     "," ";" ":" "=")
   `(operator_special
     "@")
   `(operator_field ".")
   `(operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (octave-numbers)
  (list
   `(bool_features
     "prefix_0x" "prefix_0b"
     "long_suffix" "double_suffix" "float_suffix"
     "sci_notation")
   `(separator "_")))

(tm-define (octave-inline-comment-starts)
  (list "#"))

(tm-define (octave-escape-strings)
  (list "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v"))
