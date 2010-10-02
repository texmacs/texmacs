
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-math.scm
;; DESCRIPTION : standard mathematical syntax
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language std-math)
  (:use (language std-symbols)))

(define-language std-math-operators
  (:synopsis "standard mathematical operators")

  (define Skip
    (:operator)
    (Skip-symbol :args :>))

  (define Pre
    (:operator)
    (:<lsub Script :>)
    (:<lsup Script :>)
    (:<lprime (* Prime-symbol) :>))

  (define Post
    (:operator)
    (:<rsub Script :>)
    (:<rsup Script :>)
    (:<rprime (* Prime-symbol) :>))
  
  (define Script
    (Separator Relaxed-expression Separator)
    (Separator Relaxed-expression)
    (Relaxed-expression Separator)
    Relaxed-expression)

  (define Infix
    (:operator)
    (Infix Post)
    (Pre Infix)
    Infix-symbol)

  (define Prefix
    (:operator)
    (Prefix Post)
    Prefix-symbol)

  (define Postfix
    (:operator)
    (Pre Postfix)
    Postfix-symbol)

  (define Assign-infix
    (:operator)
    (Assign-infix Post)
    (Pre Assign-infix)
    Assign-symbol)

  (define Assign-prefix
    (:operator)
    (Assign-prefix Post)
    Assign-symbol)

  (define Model-infix
    (:operator)
    (Model-infix Post)
    (Pre Model-infix)
    Model-symbol)

  (define Model-prefix
    (:operator)
    (Model-prefix Post)
    Model-symbol)

  (define Imply-infix
    (:operator)
    (Imply-infix Post)
    (Pre Imply-infix)
    Imply-symbol)

  (define Or-infix
    (:operator)
    (Or-infix Post)
    (Pre Or-infix)
    Or-symbol)

  (define And-infix
    (:operator)
    (And-infix Post)
    (Pre And-infix)
    And-symbol)

  (define Not-prefix
    (:operator)
    (Not-prefix Post)
    Not-symbol)

  (define Relation-infix
    (:operator)
    (Relation-infix Post)
    (Pre Relation-infix)
    Relation-symbol)

  (define Relation-prefix
    (:operator)
    (Relation-prefix Post)
    Relation-symbol)

  (define Arrow-infix
    (:operator)
    (Arrow-infix Post)
    (Pre Arrow-infix)
    Arrow-symbol)

  (define Arrow-prefix
    (:operator)
    (Arrow-prefix Post)
    Arrow-symbol)

  (define Union-infix
    (:operator)
    (Union-infix Post)
    (Pre Union-infix)
    Union-symbol)

  (define Exclude-infix
    (:operator)
    (Exclude-infix Post)
    (Pre Exclude-infix)
    Exclude-symbol)

  (define Intersection-infix
    (:operator)
    (Intersection-infix Post)
    (Pre Intersection-infix)
    Intersection-symbol)

  (define Plus-infix
    (:operator)
    (Plus-infix Post)
    (Pre Plus-infix)
    Plus-symbol)

  (define Plus-prefix
    (:operator)
    (Plus-prefix Post)
    Plus-prefix-symbol
    Plus-symbol)

  (define Minus-infix
    (:operator)
    (Minus-infix Post)
    (Pre Minus-infix)
    Minus-symbol)

  (define Minus-prefix
    (:operator)
    (Minus-prefix Post)
    Minus-prefix-symbol
    Minus-symbol)

  (define Times-infix
    (:operator)
    (Times-infix Post)
    (Pre Times-infix)
    Times-symbol)

  (define Times-prefix
    (:operator)
    (Times-prefix Post)
    Times-symbol)

  (define Over-infix
    (:operator)
    (Over-infix Post)
    (Pre Over-infix)
    Over-symbol)

  (define Over-prefix
    (:operator)
    (Over-prefix Post)
    Over-symbol)

  (define Power-infix
    (:operator)
    (Power-infix Post)
    (Pre Power-infix)
    Power-symbol)

  (define Power-prefix
    (:operator)
    (Power-prefix Post)
    Power-symbol)

  (define Space-infix
    (:operator)
    (+ (or Space-symbol " ")))

  (define Prefix-prefix
    (:operator)
    (Prefix-prefix Post)
    Other-prefix-symbol)

  (define Postfix-postfix
    (:operator)
    (Pre Postfix-postfix)
    Other-postfix-symbol)

  (define Big-open
    (:operator)
    (Big-open Post)
    (:<big ((not ".") :args) :>))

  (define Big-close
    (:operator)
    (Pre Big-close)
    (:<big "." :>))
  
  (define Open
    (:operator)
    (Open Post)
    Open-symbol
    (:<left :args :>))

  (define Separator
    (:operator)
    Ponctuation-symbol
    Middle-symbol
    (:<mid :args :>))

  (define Close
    (:operator)
    (Pre Close)
    Close-symbol
    (:<right :args :>)))

(define-language std-math-grammar
  (:synopsis "default syntax for mathematical formulas")

  (define Main
    (Main Separator)
    (Main ".")
    (Main "\n")
    (Main Skip)
    Relaxed-expression)

  (define Relaxed-expression
    (Relaxed-assignment Separator Relaxed-expression)
    Relaxed-assignment)

  (define Relaxed-assignment
    Assignment
    (Assign-prefix Modeling)
    (Relation-prefix Arrow)
    (Arrow-prefix Union)
    (Times-prefix Power)
    (Over-prefix Power)
    (Power-prefix Prefixed)
    Quantifier-symbol
    Infix
    Prefix
    Postfix)

  (define Expression
    (Assignment Separator Expression)
    Assignment)

  (define Assignment
    (Modeling Assign-infix Assignment)
    Modeling)

  (define Modeling
    (Sum Model-infix Quantified)
    (Model-prefix Quantified)
    Quantified)

  (define Quantified
    ((+ (Quantifier-symbol Relation)) Ponctuation-symbol Quantified)
    ((Open Quantifier-symbol Relation Close) Quantified)
    Implication)

  (define Implication
    (Implication Imply-infix Disjunction)
    Disjunction)

  (define Disjunction
    (Disjunction Or-infix Conjunction)
    Conjunction)

  (define Conjunction
    (Conjunction And-infix Negation)
    Negation)

  (define Negation
    ((+ Not-prefix) Prefixed)
    Relation)

  (define Relation
    (Relation Relation-infix Arrow)
    Arrow)

  (define Arrow
    (Arrow Arrow-infix Union)
    Union)

  (define Union
    (Union Union-infix Intersection)
    (Union Exclude-infix Intersection)
    Intersection)

  (define Intersection
    (Intersection Intersection-infix Sum)
    Sum)

  (define Sum
    (Sum Plus-infix Product)
    (Sum Minus-infix Product)
    Sum-prefix)

  (define Sum-prefix
    (Plus-prefix Sum-prefix)
    (Minus-prefix Sum-prefix)
    Product)

  (define Product
    (Product Times-infix Power)
    (Product Over-infix Power)
    Power)

  (define Power
    (Prefixed Power-infix Prefixed)
    Prefixed)

  (define Prefixed
    (Prefix-prefix Prefixed)
    (Pre Prefixed)
    (Skip Prefixed)
    (Postfixed Space-infix Prefixed)
    Postfixed)

  (define Postfixed
    (Postfixed Postfix-postfix)
    (Postfixed Post)
    (Postfixed Skip)
    (Postfixed Open Close)
    (Postfixed Open Expression Close)
    Radical)

  (define Radical
    (Open Close)
    (Open Expression Close)
    (Big-open Expression Big-close)
    Identifier
    Number
    Variable-symbol
    Suspension-symbol
    Miscellaneous-symbol
    (:<frac Expression :/ Expression :>)
    (:<sqrt Expression :>)
    (:<sqrt Expression :/ Expression :>)
    (:<wide Expression :/ :args :>)
    ((except :< Reserved-symbol) :args :>)
    :cursor)

  (define Identifier
    (+ (or (- "a" "z") (- "A" "Z"))))

  (define Number
    ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9")))))))

(define-language std-math
  (:synopsis "default semantics for mathematical formulas")
  (inherit std-symbols)
  (inherit std-math-operators)
  (inherit std-math-grammar))
