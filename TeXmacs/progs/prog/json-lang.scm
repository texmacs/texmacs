
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : json-lang.scm
;; DESCRIPTION : JSON Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog json-lang))

(tm-define (json-keywords)
  `(keywords
    (constant
      "false" "true" "null")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-update-expressions
(tm-define (json-operators)
  `(operators
    (operator "+" "-" ":" ",")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-literals-numeric-literals
(tm-define (json-numbers)
  `(numbers
    (bool_features
      "sci_notation")))

(tm-define (json-string)
  `(string
    (bool_features )
    (escape_sequences "\\" "/" "\"" "b" "f" "n" "r" "t")))
