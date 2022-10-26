
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
  ;; explicit commands
  "to" "factor" "power" "set"

  ;; latin letters
  "a" "be" "see" "de" "day" "the" "he" "eat" "each" "if" "age" "edge"
  "i" "eye" "eyes" "either" "iron" "ok" "cake" "care" "case" "all" "old"
  "an" "and" "piece" "queue" "cute"
  "are" "art" "our" "yes" "chease" "tea" "team"
  "via" "you" "vegan" "ask" "eggs" "why"

  ;; greek letters
  "grandma" "theater" "yoga" "copper" "laptop"
  "mute" "mood" "no" "new" "gnu" "knew" "site" "bye" "pie" "pipe"
  "road" "row" "role" "roll" "ciao" "towel" "tall" "toe" "toll" "town"
  "fight" "fine" "sigh" "side" "size" "kind" "sky"

  ;; letter combinations
  "ecu" "easy" "bi" "busy" "agency" "icy" "ma" "empty" "auntie" "envy"
  "annex" "pity" "peezy" "cutie" "essay" "usually" "excess" "whitey"

  ;; variants
  "pick" "plastic"
  "both" "bowl" "build" "bouquet"
  "tractor"

  ;; binary operators and relations
  "does" "play" "blessed" "please" "press" "minors"
  "time" "dancer"

  ;; textual operators
  "cosign" "lock" "luck" "look" "unlock" "timeslot"

  ;; punctuation, brackets, big operators
  "dutch" "ducks" "of" "off" "some"

  ;; fractions, subscripts, superscripts
  "offer" "oversee" "overall" "overview"
  "bishop" "sake" "subversion" "subway" "pizza" "visa"

  ;; wide accents
  "white" "head" "had" "hit" "hunt" "hurt" "pet" "cuba"

  ;; miscellaneous
  "it's" "write"

  ;; dangerous adjustments
  "by" "my" "sign" "end")

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
