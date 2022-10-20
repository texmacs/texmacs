
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
  (:use (text text-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra tables for mathematics inside text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-collection numbers english
  "zero" "one" "two" "three" "four" "five" "six" "seven"
  "eight" "nine" "ten" "hundred" "thousand" "million" "billion")

(speech-collection roman english
  "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
  "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

(speech-collection greek english
  "alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota"
  "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma" "tau"
  "upsilon" "phi" "psi" "chi" "omega")

(speech-collection modify-letters english
  "big" "small" "capital" "uppercase" "lowercase"
  "bold" "upright" "calligraphic" "fraktur" "gothic"
  "blackboard bold" "sans serif" "typewriter")

(speech-collection accept-start english
  "exponential" "logarithm" "sine" "cosine" "tangent"
  "square root")

(speech-collection accept-end english
  "prime" "dagger" "factorial")

(speech-collection reminder-dangerous english
  "be" "see" "he" "eat" "each" "if" "yes" "age" "edge" "all" "old" "an"
  "piece" "queue" "are" "our" "tea" "you" "ask" "why" "mute" "mood"
  "no" "bye" "row" "towel" "toe" "fight" "fine" "play" "time" "pick"
  "of" "off" "it's" "some" "such" "that" "power")

(speech-collection dangerous english
  "a" "be")

(speech-collection dangerous-end english
  "be" "if" "all" "are" "our" "you" "why" "no" "row"
  "of" "off" "some" "such" "that" "power")

(speech-collection forbid english
  )

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
