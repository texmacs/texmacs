
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
  "a" "be" "see" "he" "eat" "each" "if" "yes" "age" "edge"
  "either" "case" "all" "old" "an" "piece" "queue"
  "are" "art" "our" "tea" "you" "ask" "why" "mute" "mood"
  "no" "new" "knew" "bye" "road" "row" "role" "towel" "toe" "fight"
  "fine" "play" "time" "pick"
  "of" "off" "it's" "some" "power"

  "make" "did" "both" "build" "press" "please" "set")

(speech-collection dangerous english
  "is")

(speech-collection skip english
  "such" "that")

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
