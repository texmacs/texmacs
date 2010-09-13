
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
  (Main
   (Main Separator)
   (Main "\n")
   Expression)

  (Expression
   (Assignment Separator Expression)
   Assignment)

  (Assignment
   (Modeling Assign-symbol Post Assignment)
   Modeling)

  (Modeling
   (Sum Model-symbol Post Quantified)
   Quantified)

  (Quantified
   ((+ (Quantifier-symbol Relation)) Ponctuation-symbol Quantified)
   ((Open Quantifier-symbol Relation Close) Quantified)
   Implication)

  (Implication
   (Implication Imply-symbol Post Disjunction)
   Disjunction)

  (Disjunction
   (Disjunction Or-symbol Post Conjunction)
   Conjunction)

  (Conjunction
   (Conjunction And-symbol Post Relation)
   Relation)

  (Relation
   (Relation Relation-symbol Post Arrow)
   Arrow)

  (Arrow
   (Arrow Arrow-symbol Post Union)
   Union)

  (Union
   (Union Union-symbol Post Intersection)
   (Union Exclude-symbol Post Intersection)
   Intersection)

  (Intersection
   (Intersection Intersection-symbol Post Sum)
   Sum)

  (Sum
   (Sum Plus-symbol Post Product)
   (Sum Minus-symbol Post Product)
   Product)

  (Product
   (Product Times-symbol Post Power)
   (Product Over-symbol Post Power)
   Power)

  (Power
   (Big Power-symbol Post Big)
   Big)

  (Big-open
   (:<big ((not ".") :args) :>))

  (Big-close
   (:<big "." :>))

  (Big
   (Big-open Post Expression Big-close)
   Special)

  (Special
   (:<frac Expression :/ Expression :>)
   (:<sqrt Expression :>)
   (:<sqrt Expression :/ Expression :>)
   (:<wide Expression :/ :args :>)
   Prefixed)

  (Space
   (+ (or Space-symbol " ")))

  (Prefixed
   (Prefix-symbol Post Prefixed)
   (Not-symbol Post Prefixed)
   ;;(Minus-symbol Post Prefixed)
   (Pre-one Prefixed)
   (Postfixed Space Prefixed)
   Postfixed)

  (Postfixed
   (Postfixed Postfix-symbol)
   (Postfixed Post-one)
   (Postfixed Open Close)
   (Postfixed Open Expression Close)
   Radical)

  (Identifier
   (+ (or (- "a" "z") (- "A" "Z"))))

  (Number
   ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

  (Radical
   (Open Close)
   (Open Expression Close)
   Identifier
   Number
   Variable-symbol
   Suspension-symbol
   Miscellaneous-symbol
   (((not Reserved) :<) :args :>))

  (Open
   Open-symbol
   (:<left :args :>))

  (Separator
   Ponctuation-symbol
   Bar-symbol
   (:<mid :args :>))

  (Close
   Close-symbol
   (:<right :args :>))

  (Script
   Expression
   Relation-symbol
   Arrow-symbol
   Plus-symbol
   Minus-symbol
   Times-symbol
   Over-symbol
   Power-symbol)

  (Pre-one
   (:<lsub Script :>)
   (:<lsup Script :>)
   (:<lprime (* Prime-symbol) :>))

  (Post-one
   (:<rsub Script :>)
   (:<rsup Script :>)
   (:<rprime (* Prime-symbol) :>))

  (Pre
   (* Pre-one))

  (Post
   (* Post-one)))

(define-language std-math
  (inherit std-symbols)
  (inherit std-math-grammar))
