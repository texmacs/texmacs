
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scala-lang.scm
;; DESCRIPTION : Scala Language
;; COPYRIGHT   : (C) 2017-2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (scala-lang)
  (:use (prog default-lang)))

;; https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
(tm-define (parser-feature lan key)
  (:require (and (== lan "scala") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "false" "true" "null"
      "Byte" "Short" "Int" "Long" "Char" "String" "Float" "Double" "Boolean"
      "Array" "List" "Map" "Set" "Function" "Class"
      "aggregate" "collect" "map" "filter" "filterNot" "foreach" "forall" "fold"
      "foldLeft" "foldRight" "reduce" "reduceLeft" "reduceRight" "scan" "scanLeft"
      "scanRight" "zip" "unzip" "flatMap" "grouped" "groupBy"
      "IllegalArgumentException" "NullPointerException" "Exception" "RuntimeException")
    (declare_function "def")
    (declare_type "type" "class" "object" "trait")
    (declare_identifier "val" "var")
    (declare_module "package" "import")
    (keyword
      "case"  "match" ;; Pattern Matching
      "extends" "new" "with" "super" "this" ;; Object Oriented
      "override" ;; Modifier
      "abstract" "final" "sealed" "implicit" "lazy" ;; Local Modifiers
      "private" "protected" ;; Access Modifiers
      "requires" "synchronized")
    (keyword_conditional
      "break" "do" "else" "for" "if" "while")
    (keyword_control
      "throw" "catch" "finally" "return" "try" "yield")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scala") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "/" "*" "%" ;; Arith
      "|" "&" "^" ;; Bit
      "&&" "||" "!" "==" "!=" "<" ">" "<=" ">=" ;; Boolean
      ">>>" "<<" ">>"
      "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^=" ;; Assignment
      "=" ";")
    (operator_special
      ":" "=>" "::" ":::" "++"
      "+:" ":+" "++:" "/:" ":\\" "<-")
    (operator_decoration "@")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (scala-number-suffix)
  `(suffix
    (long "l" "L")
    (double "d" "D")
    (float "f" "F")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scala") (== key "number")))
  `(,(string->symbol key)
    (bool_features
     "prefix_0x" "prefix_0b"
     "sci_notation")
    ,(scala-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scala") (== key "string")))
  `(,(string->symbol key)
    (bool_features
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "'" "b" "f" "n" "r" "t")))

;; array<char> start_chars, extra_chars;
;; // The ‘$’ character is reserved for compiler-synthesized identifiers.
;; start_chars << '_' << '$';
;; extra_chars << '_';
;; identifier_parser.set_start_chars (start_chars);
;; identifier_parser.set_extra_chars (extra_chars);
