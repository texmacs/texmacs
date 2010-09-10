
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

(texmacs-module (math std-math)
  (:use (math std-symbols)))

(define-language std-math-grammar
  (Expression
   (Sum Ponctuation-symbol Expression)
   Sum)

  (Sum
   (Sum Plus-symbol Product)
   (Sum Minus-symbol Product)
   Product)

  (Product
   (Product Times-symbol Power)
   (Product Invisible-times-symbol Power)
   (Product Over-symbol Power)
   (Product Condensed-over-symbol Power)
   Power)

  (Power
   (Application :<rsup Expression :>)
   Application)

  (Application
   (Applicaton Open-symbol Expression Close-symbol)
   Radical)

  (Radical
   (Open-symbol Expression Close-symbol)
   Identifier
   Number
   Basic-symbol)

  (Identifier
   (+ (or (- "a" "z") (- "A" "Z"))))

  (Number
   ((+ (- "0" "9")) "-") (or "" ("." (+ (- "0" "9"))))))

(define-language std-math
  (inherit std-symbols)
  (inherit std-math-grammar))
