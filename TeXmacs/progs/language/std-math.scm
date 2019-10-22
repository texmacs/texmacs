
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

  (define Pre
    (:selectable inside)
    (:<lsub Script :>)
    (:<lsup Script :>)
    (:<lprime (* Prime-symbol) :>))

  (define Post
    (:selectable inside)
    (:<rsub Script :>)
    (:<rsup Script :>)
    (:<rprime (* Prime-symbol) :>))
  
  (define Script
    (Separator Relaxed-expressions Separator)
    (Separator Relaxed-expressions)
    (Relaxed-expressions Separator)
    Relaxed-expressions)

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

  (define Models-infix
    (:operator)
    (Models-infix Post)
    (Pre Models-infix)
    Models-symbol
    Modeled-symbol)

  (define Models-prefix
    (:operator)
    (Models-prefix Post)
    Models-symbol)

  (define Imply-infix
    (:operator associative)
    (Imply-infix Post)
    (Pre Imply-infix)
    Imply-symbol)

  (define Or-infix
    (:operator associative)
    (Or-infix Post)
    (Pre Or-infix)
    Or-symbol)

  (define And-infix
    (:operator associative)
    (And-infix Post)
    (Pre And-infix)
    And-symbol)

  (define Not-prefix
    (:operator)
    (Not-prefix Post)
    Not-symbol)

  (define Relation-infix
    (:operator associative)
    (Relation-infix Post)
    (Pre Relation-infix)
    Relation-symbol
    Arrow-symbol
    (:<long-arrow :any :/ Expression :>)
    (:<long-arrow :any :/ Slot :/ Expression :>))

  (define Relation-prefix
    (:operator)
    (Relation-prefix Post)
    Relation-symbol
    Arrow-symbol
    (:<long-arrow :any :/ Expression :>)
    (:<long-arrow :any :/ Slot :/ Expression :>))

  (define Union-infix
    (:operator associative)
    (Union-infix Post)
    (Pre Union-infix)
    Union-symbol)

  (define Exclude-infix
    (:operator)
    (Exclude-infix Post)
    (Pre Exclude-infix)
    Exclude-symbol)

  (define Intersection-infix
    (:operator associative)
    (Intersection-infix Post)
    (Pre Intersection-infix)
    Intersection-symbol)

  (define Plus-infix
    (:operator associative)
    (Plus-infix Post)
    (Pre Plus-infix)
    Plus-symbol)

  (define Plus-prefix
    (:operator)
    (Plus-prefix Post)
    Plus-prefix-symbol
    Plus-symbol)

  (define Minus-infix
    (:operator anti-associative)
    (Minus-infix Post)
    (Pre Minus-infix)
    Minus-symbol)

  (define Minus-prefix
    (:operator)
    (Minus-prefix Post)
    Minus-prefix-symbol
    Minus-symbol)

  (define Times-infix
    (:operator associative)
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
    (:operator associative)
    Spacing-symbol)

  (define Ponctuation-infix
    (:operator associative)
    Ponctuation-symbol
    ".")

  (define Prefix-prefix
    (:operator)
    (Prefix-prefix Post)
    Other-prefix-symbol
    Unary-operator-glyph-symbol
    (:<Prefix :args :>))

  (define Postfix-postfix
    (:operator)
    (Pre Postfix-postfix)
    Other-postfix-symbol
    (:<Postfix :args :>))
 
  (define Open
    (:operator)
    (Open Post)
    Open-symbol
    (:<left :args :>))

  (define Close
    (:operator)
    (Pre Close)
    Close-symbol
    (:<right :args :>))

  (define Middle
    (:operator associative)
    ;;(Middle Post)
    ;;(Pre Middle)
    Middle-symbol
    (:<mid :args :>))

  (define Comma
    (:operator associative)
    Ponctuation-symbol)

  (define Separator
    (:operator associative)
    Ponctuation-symbol
    Middle-symbol
    (:<mid :args :>))
 
  (define Big-separator
    (:operator)
    (Big-separator Post)
    (:<big Big-separator-symbol :>))
 
  (define Big-or
    (:operator)
    (Big-or Post)
    (:<big Big-or-symbol :>))
 
  (define Big-and
    (:operator)
    (Big-and Post)
    (:<big Big-and-symbol :>))
 
  (define Big-union
    (:operator)
    (Big-union Post)
    (:<big Big-union-symbol :>))
 
  (define Big-intersection
    (:operator)
    (Big-intersection Post)
    (:<big Big-intersection-symbol :>))
 
  (define Big-sum
    (:operator)
    (Big-sum Post)
    (:<big Big-sum-symbol :>))
 
  (define Big-product
    (:operator)
    (Big-product Post)
    (:<big Big-product-symbol :>)))

(define-language std-math-grammar
  (:synopsis "default syntax for mathematical formulas")

  (define Main
    (Main Separator)
    (Main ".")
    (Main "\n")
    Relaxed-expressions)

  (define Strict
    (Strict "\n")
    Relaxed-expressions)

  (define Relaxed-expressions
    (Relaxed-expressions Middle Relaxed-expression-list)
    Relaxed-expression-list)

  (define Relaxed-expression-list
    Informal-relation
    (Relaxed-expression-list Comma Relaxed-expression)
    Relaxed-expression)

  (define Relaxed-expression
    Assignment
    (Assign-prefix Modeling)
    (Relation-prefix Union)
    (Times-prefix Power)
    (Over-prefix Power)
    (Power-prefix Prefixed)
    Quantifier-prefix
    Quantifier-prefix-symbol
    Prime-symbol
    Infix
    Prefix
    Postfix
    (:<Prefix :args :>)
    (:<Postfix :args :>))

  (define Expressions
    (Expressions Middle Expression-list)
    Expression-list)

  (define Expression-list
    Informal-relation
    (Expression-list Comma Expression)
    Expression)

  (define Expression
    Assignment
    Quantifier-prefix-symbol
    Prime-symbol
    Infix
    Prefix
    Postfix)

  (define Informal-relation
    (Union-list Relation-infix Union))

  (define Union-list
    (Union-list Comma Union)
    (Union Comma Union))
  
  (define Assignment
    (Modeling Assign-infix Assignment)
    Modeling)

  (define Modeling
    (Sum Models-infix Quantified)
    (Models-prefix Quantified)
    Quantified)

  (define Quantifier-prefix-symbol
    (:operator)
    Quantifier-symbol)

  (define Quantifier-prefix
    (Quantifier-prefix-symbol Relation))

  (define Quantifier-prefixes
    (+ Quantifier-prefix))

  (define Quantifier-fenced
    (:focus disallow)
    (Open Quantifier-prefixes Close)
    (:<around :any :/ Quantifier-prefixes :/ :any :>)
    (:<around* :any :/ Quantifier-prefixes :/ :any :>))

  (define Quantified
    (Quantifier-prefixes Ponctuation-infix Quantified)
    (Quantifier-fenced Space-infix Quantified)
    (Quantifier-fenced Quantified)
    Quantifier-fenced
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
    (Relation Relation-infix Union)
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
    (Big-separator Expression)
    (Big-or Conjunction)
    (Big-and Negation)
    (Big-union Intersection)
    (Big-intersection Sum)
    (Big-sum Sum-prefix)
    (Big-product Power)
    (Prefix-prefix Prefixed)
    (Pre Prefixed)
    (Postfixed Space-infix Prefixed)
    Postfixed)

  (define Fenced-postfix
    (:focus disallow)
    (Open Close)
    (Open Expressions Close)
    (:<around :any :/ (* Post) (* Pre) :/ :any :>)
    (:<around* :any :/ (* Post) (* Pre) :/ :any :>)
    (:<around :any :/ (* Post) Expressions (* Pre) :/ :any :>)
    (:<around* :any :/ (* Post) Expressions (* Pre) :/ :any :>))

  (define Fenced
    Fenced-postfix)

  (define Restrict
    (:focus disallow)
    ((or "|" (:<mid "|" :>))
     (+ (or (:<rsub Script :>) (:<rsup Script :>)))))

  (define Postfixed
    (Postfixed Postfix-postfix)
    (Postfixed Post)
    (Postfixed Fenced-postfix)
    (Postfixed Restrict)
    Radical)

  (define Slot
    Expression
    "")

  (define Cell
    (Main Or-infix)
    (Main And-infix)
    (Main Union-infix)
    (Main Exclude-infix)
    (Main Intersection-infix)
    (Main Plus-infix)
    (Main Minus-infix)
    Main
    Separator
    "")

  (define Row
    (:<row Cell (* (:/ Cell)) :>))

  (define Radical
    Fenced
    (:<big-around :any :/ (* Post) Expressions :>)
    Identifier
    Number
    Letter-symbol
    Suspension-symbol
    Miscellaneous-symbol
    Unary-operator-glyph-symbol
    (:<frac Expression :/ Slot :>)
    (:<sqrt Expression :>)
    (:<sqrt Expression :/ Slot :>)
    (:<wide Expression :/ :args :>)
    (:<table Row (* (:/ Row)) :>)
    ((except :< Reserved-symbol) :args :>)
    :cursor)

  (define Identifier
    (+ (or (- "a" "z") (- "A" "Z"))))

  (define Number
    ((+ (- "0" "9")) (or "." "<comma>") (+ (- "0" "9")))
    ((+ (- "0" "9")) (or "." "<comma>"))
    ((or "." "<comma>") (+ (- "0" "9")))
    (+ (- "0" "9"))
    (or "." "<comma>")))

(define-language std-math
  (:synopsis "default semantics for mathematical formulas")
  (inherit std-symbols)
  (inherit std-math-operators)
  (inherit std-math-grammar))
