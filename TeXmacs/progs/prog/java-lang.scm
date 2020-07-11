
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : java-lang.scm
;; DESCRIPTION : Java Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog java-lang))

(tm-define (java-keywords)
  (list
   `(constant
     "false" "true" "null"
     "boolean" "byte" "char" "double" "float" "int" "long" "short" "void"
     "IllegalArgumentException" "NullPointerException" "Exception" "RuntimeException")
   `(declare_type "class" "interface" "enum")
   `(declare_identifier "val" "var")
   `(declare_module "package" "import")
   `(keyword
     "new" "extends" "implements" "super" "this" "instanceof" ;; Object Oriented
     "default" "native" "abstract" "final" "static" "volatile" "transient";; Local Modifiers
     "private" "protected" "public";; Access Modifiers
     "throws" "synchronized")
   `(keyword_conditional
     "break" "continue" "do" "else" "for" "if" "while" "goto" "switch" "case")
   `(keyword_control
     "throw" "catch" "finally" "return" "try" "yield")))


(tm-define (java-operators)
  (list
   `(operator
     "+" "-" "/" "*" "%" ;; Arith
     "|" "&" "^" ;; Bit
     "&&" "||" "!"
     "<less><less>" "<gtr><gtr>" "==" "!="
     "<less>" "<gtr>" "<less>=" "<gtr>="
     "&&" "||" "!" "==" "!=" ;; Boolean
     "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^=" ;; Assignment
     "=" ":")
   `(operator_special "-<gtr>")
   `(operator_decoration "@")
   `(operator_field "." "::")
   `(operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (java-numbers)
  (list
   `(bool_features
     "prefix_0x" "prefix_0b"
     "long_suffix" "double_suffix" "float_suffix"
     "sci_notation")))

(tm-define (java-inline-comment-starts)
  (list "//"))

(tm-define (java-escape-strings)
  (list "\\" "\"" "'" "b" "f" "n" "r" "t"))

;; TODO:
;; escaped_char_parser.support_octal_upto_3_digits (true);
;; escaped_char_parser.support_hex_with_16_bits (true);
