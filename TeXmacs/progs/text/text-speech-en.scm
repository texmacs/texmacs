
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

  ("find" (interactive-search))
  ("search" (interactive-search))
  ("previous match" (search-next-match #f))
  ("next match" (search-next-match #t))
  ("spell" (interactive-spell))
  ("replace" (interactive-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-adjust english text-hack
  ("mb" "m be")
  ("nb" "n be")
  ("rb" "r be"))

(speech-adjust english text
  ("call ma" ",")
  ("andrew" "undo"))
