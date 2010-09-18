
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
  (define Main
    (Main Separator)
    (Main ".")
    (Main "\n")
    Expression)

  (define Expression
    (Assignment Separator Expression)
    Assignment)

  (define Assignment
    (Modeling Assign-symbol Post Assignment)
    Modeling)

  (define Modeling
    (Sum Model-symbol Post Quantified)
    Quantified)

  (define Quantified
    ((+ (Quantifier-symbol Relation)) Ponctuation-symbol Quantified)
    ((Open Quantifier-symbol Relation Close) Quantified)
    Implication)

  (define Implication
    (Implication Imply-symbol Post Disjunction)
    Disjunction)

  (define Disjunction
    (Disjunction Or-symbol Post Conjunction)
    Conjunction)

  (define Conjunction
    (Conjunction And-symbol Post Relation)
    Relation)

  (define Relation
    (Relation Relation-symbol Post Arrow)
    Arrow)

  (define Arrow
    (Arrow Arrow-symbol Post Union)
    Union)

  (define Union
    (Union Union-symbol Post Intersection)
    (Union Exclude-symbol Post Intersection)
    Intersection)

  (define Intersection
    (Intersection Intersection-symbol Post Sum)
    Sum)

  (define Sum
    (Sum Plus-symbol Post Product)
    (Sum Minus-symbol Post Product)
    Product)

  (define Product
    (Product Times-symbol Post Power)
    (Product Over-symbol Post Power)
    Power)

  (define Power
    (Big Power-symbol Post Big)
    Big)

  (define Big-open
    (:<big ((not ".") :args) :>))

  (define Big-close
    (:<big "." :>))

  (define Big
    (Big-open Post Expression Big-close)
    Special)

  (define Special
    (:<frac Expression :/ Expression :>)
    (:<sqrt Expression :>)
    (:<sqrt Expression :/ Expression :>)
    (:<wide Expression :/ :args :>)
    Prefixed)

  (define Space
    (+ (or Space-symbol " ")))

  (define Prefixed
    (Prefix-symbol Post Prefixed)
    (Not-symbol Post Prefixed)
    ;;(Minus-symbol Post Prefixed)
    (Pre-one Prefixed)
    (Postfixed Space Prefixed)
    Postfixed)

  (define Postfixed
    (Postfixed Postfix-symbol)
    (Postfixed Post-one)
    (Postfixed Open Close)
    (Postfixed Open Expression Close)
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
    (((not Reserved) :<) :args :>))

  (define Open
    Open-symbol
    (:<left :args :>))

  (define Separator
    Ponctuation-symbol
    Bar-symbol
    (:<mid :args :>))

  (define Close
    Close-symbol
    (:<right :args :>))

  (define Script
    Expression
    Relation-symbol
    Arrow-symbol
    Plus-symbol
    Minus-symbol
    Times-symbol
    Over-symbol
    Power-symbol)

  (define Pre-one
    (:<lsub Script :>)
    (:<lsup Script :>)
    (:<lprime (* Prime-symbol) :>))

  (define Post-one
    (:<rsub Script :>)
    (:<rsup Script :>)
    (:<rprime (* Prime-symbol) :>))

  (define Pre
    (* Pre-one))
  
  (define Post
    (* Post-one)))

(define-language std-math
  (inherit std-symbols)
  (inherit std-math-grammar))
