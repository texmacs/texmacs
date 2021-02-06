
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

(texmacs-module (prog octave-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "octave") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "false" "true")
    (declare_type "function" "endfunction" "class")
    (declare_module "pkg")
    (keyword_conditional
      "break" "continue" "do" "else" "for" "endfor" "if" "endif"
      "while" "endwhile" "switch" "case")
    (keyword_control
      "catch" "try")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "octave") (== key "operator")))
  `(,(string->symbol key)
    (operator "," ";" ":" "=")
    (operator_special "@")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (octave-number-suffix)
  `(suffix
    (imaginary "j" "J")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "octave") (== key "number")))
  `(,(string->symbol key)
    (bool_features
     "prefix_0x" "prefix_0b"
     "sci_notation")
    (separator "_")
    ,(octave-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "octave") (== key "string")))
  `(,(string->symbol key)
    (bool_features 
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v")))

; See: https://octave.org/doc/v6.1.0/Single-Line-Comments.html
(tm-define (parser-feature lan key)
  (:require (and (== lan "octave") (== key "comment")))
  `(,(string->symbol key)
    (inline "#" "%")))
