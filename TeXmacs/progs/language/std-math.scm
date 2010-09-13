
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
   (Main Ponctuation-symbol)
   Expression)

  (Expression
   (Modeling Ponctuation-symbol Expression)
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
   (Relation Relation-symbol Sum)
   Sum)

  (Sum
   (Sum Plus-symbol Product)
   (Sum Minus-symbol Product)
   Product)

  (Product
   (Product Times-symbol Special)
   (Product Over-symbol Special)
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

  (Operation
   (Application Space-symbol Operation)
   Application)

  (Application
   (Application Open Expression Close)
   Radical)

  (Radical
   (Open Expression Close)
   Identifier
   Number
   Variable-symbol
   Miscellaneous-symbol)

  (Open
   Open-symbol
   (:<left Open-symbol :>))

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
