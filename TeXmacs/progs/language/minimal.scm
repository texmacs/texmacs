
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : minimal.scm
;; DESCRIPTION : syntax of a minimal test language
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language minimal))

(define-language minimal-operators
  (:synopsis "operators for a minimal test language")

  (define Spc
    (:operator)
    (* (or " " "\t" "\n")))

  (define Space
    (:operator)
    (+ (or " " "\t" "\n")))

  (define End
    (:operator)
    (Spc ";"))

  (define If-prefix
    (:operator)
    (:highlight keyword)
    ("if" Space))

  (define Then-infix
    (:operator)
    (:highlight keyword)
    (Space "then" Space))

  (define While-prefix
    (:operator)
    (:highlight keyword)
    ("while" Space))

  (define Do-infix
    (:operator)
    (:highlight keyword)
    (Space "do" Space))

  (define Declare-infix
    (:operator)
    (Spc "==" Spc))

  (define Comma-infix
    (:operator)
    (Spc "," Spc))

  (define Assign-infix
    (:operator)
    (Spc ":=" Spc))

  (define Relation-infix
    (:operator)
    (Spc (or "=" "!=" "<less>" "<lesseq>" "<gtr>" "<gtreq>") Spc))

  (define Plus-infix
    (:operator)
    (Spc "+" Spc))

  (define Minus-infix
    (:operator)
    (Spc "-" Spc))

  (define Times-infix
    (:operator)
    (Spc "*" Spc))

  (define Over-infix
    (:operator)
    (Spc "/" Spc))

  (define Power-infix
    (:operator)
    (Spc "^" Spc))

  (define Cardial-prefix
    (:operator)
    ("#" Spc))

  (define Factorial-postfix
    (:operator)
    (Spc "!"))

  (define Open
    (:operator)
    ("(" Spc))

  (define Close
    (:operator)
    (Spc ")"))

  (define Open-close
    ("(" Spc ")"))

  (define Error-curly
    (:operator)
    (:highlight error)
    (* (or ("{" Error-curly "}")
	   (except :char (or "{" "}")))))

  (define Error-semi
    (:operator)
    (:highlight error)
    (* (or ("{" Error-curly "}")
	   (except :char (or "{" "}" ";"))))))

(define-language minimal-grammar
  (:synopsis "grammar for a minimal test language")

  (define Main
    (Spc Instruction Main)
    Spc)

  (define Instruction
    ("{" Main "}")
    ("{" Error-curly "}")
    (Lhs Declare-infix Instruction)
    (If-prefix Expression Then-infix Instruction)
    (While-prefix Expression Do-infix Instruction)
    ";"
    (Expression End)
    (Error-semi End))
  
  (define Lhs
    (Lhs Spc Open Expression Close)
    (Lhs Spc Open-Close)
    Lhs-radical)

  (define Lhs-radical
    (:highlight declare)
    Radical)

  (define Expression
    (Assignment Comma-infix Expression)
    Assignment)

  (define Assignment
    (Relation Assign-infix Assignment)
    Relation)

  (define Relation
    (Relation Relation-infix Sum)
    Sum)

  (define Sum
    (Sum Plus-infix Product)
    (Sum Minus-infix Product)
    Product)

  (define Product
    (Product Times-infix Power)
    (Product Over-infix Power)
    Power)

  (define Power
    (Prefixed Power-infix Prefixed)
    Prefixed)

  (define Prefixed
    (Cardial-prefix Prefixed)
    Postfixed)

  (define Postfixed
    (Postfixed Factorial-postfix)
    (Postfixed Spc Open Expression Close)
    (Postfixed Spc Open-close)
    Radical)

  (define Identifier
    (:highlight variable_identifier)
    (+ (or (- "a" "z") (- "A" "Z"))))

  (define Number
    (:highlight constant_number)
    ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

  (define Radical
    (Open Expression Close)
    Open-close
    Identifier
    Number))

(define-language minimal
  (:synopsis "syntax for a minimal test language")
  (inherit minimal-operators)
  (inherit minimal-grammar))
