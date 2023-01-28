
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : javascript-lang.scm
;; DESCRIPTION : JavaScript Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (javascript-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "javascript") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "false" "true" "null" "undefined" "NaN" "Infinity"
      "void" "Array" "Boolean" "BigInt" "Map" "Object" "String" "Set")
    (declare_type "class" "interface" "enum")
    (declare_identifier "let" "var")
    (declare_module "as" "default" "export" "from" "import" "package")
    (keyword
      "new" "extends" "implements" "super" "this" "instanceof" ;; Object Oriented
      "abstract" "const" "static" "private" "protected" "public" ;; Modifiers
      "function" "delete" "in" "arguments" "async" "await")
    (keyword_conditional
      "else"  "if" "goto" "switch" "case")
    (keyword_control
      "await" "break" "catch" "continue" "debugger" "do"
      "for" "finally" "return" "throw" "try" "while" "with")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-update-expressions
(tm-define (parser-feature lan key)
  (:require (and (== lan "javascript") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "++" "--" ;; Update Expressions
      "+" "-" "~" "!" ;; Unary Operators
      "**" ;; Exponentiation Operator
      "/" "*" "%" ;; Multiplicative Operators
      ;; Additive Operators (already in Unary Operators)
      "<<" ">>" ">>>" ;; Bitwise Shift Operators
      "<" ">" "<=" ">=" "instanceof" "in" ;; Relational Operators
      ;; Assignment Operators
      "="
      "+=" "-=" "/=" "*=" "%="  "**="
      "&=" "^=" "|="
      "<<=" ">>=" ">>>="
      "==" "!=" "===" "!==" ;; Equality Operators
      "|" "&" "^" ;; Binary Bitwise Operators
      "&&" "||" "!" ;; Binary Logical Operators
      "?" ":" ;; Conditional Operator
      "," ;; Comma Operator
      ";")
    (operator_special "->")
    (operator_decoration "@")
    (operator_field "." "::")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-literals-numeric-literals
(tm-define (parser-feature lan key)
  (:require (and (== lan "javascript") (== key "number")))
  `(,(string->symbol key)
    (bool_features
      "prefix_0x" "prefix_0b" "prefix_0o"
      "sci_notation")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "javascript") (== key "string")))
  `(,(string->symbol key)
    (bool_features
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "'" "b" "f" "n" "r" "v" "t")))
