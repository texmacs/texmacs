
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
   Expression)

  (Expression
   (Assignment Separator Expression)
   Assignment)

  (Assignment
   (Modeling Assign-symbol Assignment)
   Modeling)

  (Modeling
   (Sum Model-symbol Quantified)
   Quantified)

  (Quantified
   ((+ (Quantifier-symbol Relation)) Ponctuation-symbol Quantified)
   ((Open Quantifier-symbol Relation Close) Quantified)
   Implication)

  (Implication
   (Implication Imply-symbol Disjunction)
   Disjunction)

  (Disjunction
   (Disjunction Or-symbol Conjunction)
   Conjunction)

  (Conjunction
   (Conjunction And-symbol Relation)
   Relation)

  (Relation
   (Relation Relation-symbol Arrow)
   Arrow)

  (Arrow
   (Arrow Arrow-symbol Union)
   Union)

  (Union
   (Union Union-symbol Intersection)
   (Union Exclude-symbol Intersection)
   Intersection)

  (Intersection
   (Intersection Intersection-symbol Sum)
   Sum)

  (Sum
   (Sum Plus-symbol Product)
   (Sum Minus-symbol Product)
   Product)

  (Product
   (Product Times-symbol Power)
   (Product Over-symbol Power)
   Power)

  (Power
   (Big Power-symbol Big)
   Big)

  (Big-open
   (:<big Big-symbol-variant :>))

  (Big-modifier
   (:<rsub Expression :>)
   (:<rsup Expression :>)
   (:<rprime (* Prime-symbol) :>))

  (Big-close
   (:<big "." :>))

  (Big
   (Big-open (* Big-modifier) Expression Big-close)
   Special)

  (Special
   (:<frac Expression :/ Expression :>)
   (:<sqrt Expression :>)
   (:<sqrt Expression :/ Expression :>)
   Prefixed)

  (Prefixed
   (Prefix-symbol Prefixed)
   (Not-symbol Prefixed)
   (Minus-symbol Prefixed)
   (Postfixed :<lsub Expression :>)
   (Postfixed :<lsup Expression :>)
   (Postfixed :<lprime (* Prime-symbol) :>)
   Postfixed)

  (Postfixed
   (Postfixed Postfix-symbol)
   (Postfixed :<rsub Expression :>)
   (Postfixed :<rsup Expression :>)
   (Postfixed :<rprime (* Prime-symbol) :>)
   Operation)

  (Space
   (* (or Space-symbol " ")))

  (Operation
   (Application Space Operation)
   Application)

  (Application
   (Application Open Expression Close)
   Radical)

  (Radical
   (Open Expression Close)
   Identifier
   Number
   Variable-symbol
   Suspension-symbol
   Miscellaneous-symbol)

  (Open
   Open-symbol
   (:<left Open-symbol :>))

  (Separator
   Ponctuation-symbol
   Bar-symbol
   (:<mid Bar-symbol :>))

  (Close
   Close-symbol
   (:<right Close-symbol :>))

  (Identifier
   (+ (or (- "a" "z") (- "A" "Z"))))

  (Number
   ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9")))))))

(define-language std-math
  (inherit std-symbols)
  (inherit std-math-grammar))
