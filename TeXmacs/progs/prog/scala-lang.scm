
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scala-lang.scm
;; DESCRIPTION : Scala Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scala-lang))

;; https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
(tm-define (scala-keywords)
  (list
   `(constant
     "false" "true" "null"
     "Byte" "Short" "Int" "Long" "Char" "String" "Float" "Double" "Boolean"
     "Array" "List" "Map" "Set" "Function" "Class"
     "aggregate" "collect" "map" "filter" "filterNot" "foreach" "forall" "fold"
     "foldLeft" "foldRight" "reduce" "reduceLeft" "reduceRight" "scan" "scanLeft"
     "scanRight" "zip" "unzip" "flatMap" "grouped" "groupBy"
     "IllegalArgumentException" "NullPointerException" "Exception" "RuntimeException")
   `(declare_function "def")
   `(declare_type "type" "class" "object" "trait")
   `(declare_identifier "val" "var")
   `(declare_module "package" "import")
   `(keyword
     "case"  "match" ;; Pattern Matching
     "extends" "new" "with" "super" "this" ;; Object Oriented
     "override" ;; Modifier
     "abstract" "final" "sealed" "implicit" "lazy" ;; Local Modifiers
     "private" "protected" ;; Access Modifiers
     "requires" "synchronized")
   `(keyword_conditional
     "break" "do" "else" "for" "if" "while")
   `(keyword_control
     "throw" "catch" "finally" "return" "try" "yield")))

(tm-define (scala-operators)
  (list
   `(operator
     "+" "-" "/" "*" "%" ;; Arith
     "|" "&" "^" ;; Bit
     "&&" "||" "!" "==" "!=";; Boolean
     "<less>" "<gtr>" "<less>=" "<gtr>="
     "<gtr><gtr><gtr>" "<less><less>" "<gtr><gtr>"
     "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^=" ;; Assignment
     "=")
   `(operator_special
     ":" "=<gtr>" "::" ":::" "++"
     "+:" ":+" "++:" "/:" ":\\" "<less>-")
   `(operator_decoration "@")
   `(operator_field ".")
   `(operator_openclose "{" "[" "(" ")" "]" "}")))
