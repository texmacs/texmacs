
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

(define-language minimal
  (:synopsis "syntax for a minimal test language")

  (define Main
    (Spc Definition Main)
    Spc)
  
  (define Definition
    (Lhs Spc "==" Spc Expression Spc ";"))

  (define Lhs
    (Lhs Spc "(" Spc ")")
    (Lhs Spc "(" Spc Expression Spc ")")
    Lhs-radical)

  (define Lhs-radical
    (:highlight declare)
    Radical)

  (define Expression
    (Assignment Spc "," Spc Expression)
    Assignment)

  (define Assignment
    (Relation Spc ":=" Spc Assignment)
    Relation)

  (define Relation
    (Relation Spc (or "=" "!=" "<less>" "<lesseq>" "<gtr>" "<gtreq>") Spc Sum)
    Sum)

  (define Sum
    (Sum Spc "+" Spc Product)
    (Sum Spc "-" Spc Product)
    Product)

  (define Product
    (Product Spc "*" Spc Power)
    (Product Spc "/" Spc Power)
    Power)

  (define Power
    (Prefixed Spc "^" Spc Prefixed)
    Prefixed)

  (define Prefixed
    ("#" Spc Prefixed)
    Postfixed)

  (define Postfixed
    (Postfixed Spc "!")
    (Postfixed Spc "(" Spc ")")
    (Postfixed Spc "(" Spc Expression Spc ")")
    Radical)

  (define Identifier
    (:highlight variable_identifier)
    (+ (or (- "a" "z") (- "A" "Z"))))

  (define Number
    (:highlight constant_number)
    ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

  (define Radical
    ("(" Spc ")")
    ("(" Spc Expression Spc ")")
    Identifier
    Number)

  (define Spc
    (* (or " " "\t" "\n"))))
