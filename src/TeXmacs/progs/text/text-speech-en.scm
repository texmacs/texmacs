
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-speech-en.scm
;; DESCRIPTION : textual editing using English speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-speech-en)
  (:use (text text-speech)
        (math math-speech-en)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra tables for mathematics inside text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-collection prefix english
  "big" "small" "capital" "uppercase" "lowercase"
  "bold" "upright" "calligraphic" "fraktur" "gothic"
  "blackboard bold" "sans serif" "typewriter")

(speech-collection prefix english
  "exponential" "logarithm" "sine" "cosine" "tangent"
  "square root")

(speech-collection postfix english
  "prime" "dagger")

(speech-collection dangerous english
  "to"
  
  "a" "be" "see" "de" "the" "he" "eat" "each" "if" "yes" "age" "edge"
  "i" "eye" "eyes" "either" "ok" "case" "cake" "all" "old"
  "an" "and" "piece" "queue" "cute"
  "are" "art" "our" "tea" "team" "you" "ask" "why"

  "theater" "copper" "mute" "mood" "no" "new" "gnu" "knew" "bye" "pie"
  "road" "row" "role" "roll" "ciao" "towel" "tall" "toe" "toll" "town"
  "fight" "fine" "sigh" "size" "kind" "sky"

  "plastic" "both" "build" "bouquet" "tractor"
  "does" "play" "blessed" "please" "press" "time"
  "time" "sake" "subway" "pizza"
  "cosign" "pick" "dutch" "ducks"
  "of" "off" "some" "offer" "oversee" "it's"
  "lock" "luck" "look" "unlock" "timeslot"
  "white" "head" "had" "hit" "hunt" "hurt"
  "cuba" "write"

  "sign" "end"

  "factor" "power" "set"

  "make" "did")

(speech-collection dangerous english
  "is")

(speech-collection skip english
  "such" "that")

(speech-collection math-mode english
  "math" "maths" "mathematics" "formula")

(speech-collection text-mode english
  "text")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General speech commands for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map english std-text
  ("math" (speech-inline 'math))
  ("maths" (speech-inline 'math))
  ("formula" (speech-inline 'math))
  ("numbered equation" (make-equation))
  ("displayed formula" (make-equation*))
  ("equation" (make-equation*))
  ("equations" (make-eqnarray*))

  ("abbreviation" (speech-inline 'abbr))
  ("description" (make-tmlist 'description))
  ("enumerate" (make-tmlist 'enumerate))
  ("itemize" (make-tmlist 'itemize))
  ("emphasize" (speech-inline 'em))
  ("name" (speech-inline 'name))
  ("sample" (speech-inline 'samp))
  ("strong" (speech-inline 'strong))
  ("verbatim" (speech-inline 'verbatim))
  ("item" (make-item))
  ("next item" (make-item))
  ("chapter" (make-section 'chapter))
  ("section" (make-section 'section))
  ("subsection" (make-section 'subsection))
  ("subsubsection" (make-section 'subsubsection))
  ("paragraph" (make-section 'paragraph))
  ("subparagraph" (make-section 'subparagraph))

  ("proof" (speech-proof))

  ("find" (interactive-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust english text-hack
  ("mb" "m be")
  ("nb" "n be")
  ("rb" "r be"))
