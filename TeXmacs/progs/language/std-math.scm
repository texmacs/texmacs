
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

(define-language std-math-grammar
  (:synopsis "default syntax for mathematical formulas")

  (define Main
    (Main Separator)
    (Main ".")
    (Main "\n")
    Expression)

  (define Expression
    (Assignment Separator Expression)
    Assignment)

  (define Assignment
    (Modeling Assign-infix Assignment)
    Modeling)

  (define Modeling
    (Sum Model-infix Quantified)
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
    (Conjunction And-infix Relation)
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
    Product)

  (define Product
    (Product Times-infix Power)
    (Product Over-infix Power)
    Power)

  (define Power
    (Big Power-infix Big)
    Big)

  (define Big
    (Big-open Expression Big-close)
    Special)

  (define Special
    (:<frac Expression :/ Expression :>)
    (:<sqrt Expression :>)
    (:<sqrt Expression :/ Expression :>)
    (:<wide Expression :/ :args :>)
    Prefixed)

  (define Prefixed
    (Prefix-prefix Prefixed)
    (Not-prefix Prefixed)
    ;;(Minus-prefix Prefixed)
    (Pre Prefixed)
    (Postfixed Space-infix Prefixed)
    Postfixed)

  (define Postfixed
    (Postfixed Postfix-postfix)
    (Postfixed Post)
    (Postfixed (* Pre) Open Close)
    (Postfixed (* Pre) Open Expression Close)
    Radical)

  (define Identifier
    (+ (or (- "a" "z") (- "A" "Z"))))

  (define Number
    ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

  (define Radical
    (Open Close)
    (Open Expression Close)
    Identifier
    Number
    Variable-symbol
    Suspension-symbol
    Miscellaneous-symbol
    (((not Reserved) :<) :args :>)
    :cursor)
  
  (define Script
    Expression
    Relation-symbol
    Arrow-symbol
    Plus-symbol
    Minus-symbol
    Times-symbol
    Over-symbol
    Power-symbol)

  (define Pre
    (:<lsub Script :>)
    (:<lsup Script :>)
    (:<lprime (* Prime-symbol) :>))

  (define Post
    (:<rsub Script :>)
    (:<rsup Script :>)
    (:<rprime (* Prime-symbol) :>))

  (define Assign-infix
    (:operator)
    (Assign-infix Post)
    (Pre Assign-infix)
    Assign-symbol)

  (define Model-infix
    (:operator)
    (Model-infix Post)
    (Pre Model-infix)
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

  (define Relation-infix
    (:operator)
    (Relation-infix Post)
    (Pre Relation-infix)
    Relation-symbol)

  (define Arrow-infix
    (:operator)
    (Arrow-infix Post)
    (Pre Arrow-infix)
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

  (define Minus-infix
    (:operator)
    (Minus-infix Post)
    (Pre Minus-infix)
    Minus-symbol)

  (define Times-infix
    (:operator)
    (Times-infix Post)
    (Pre Times-infix)
    Times-symbol)

  (define Over-infix
    (:operator)
    (Over-infix Post)
    (Pre Over-infix)
    Over-symbol)

  (define Power-infix
    (:operator)
    (Power-infix Post)
    (Pre Power-infix)
    Power-symbol)

  (define Space-infix
    (:operator)
    (+ (or Space-symbol " ")))

  (define Prefix-prefix
    (:operator)
    (Prefix-prefix Post)
    Prefix-symbol)

  (define Not-prefix
    (:operator)
    (Not-prefix Post)
    Not-symbol)

  (define Postfix-postfix
    (:operator)
    (Pre Postfix-postfix)
    Postfix-symbol)

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
    Bar-symbol
    (:<mid :args :>))

  (define Close
    (:operator)
    (Pre Close)
    Close-symbol
    (:<right :args :>)))

(define-language std-math
  (:synopsis "default semantics for mathematical formulas")
  (inherit std-symbols)
  (inherit std-math-grammar))
