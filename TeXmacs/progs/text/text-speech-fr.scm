
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-speech-fr.scm
;; DESCRIPTION : textual editing using French speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-speech-fr)
  (:use (text text-speech)
        (math math-speech-fr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra tables for mathematics inside text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-collection number french
  "zéro" "un" "deux" "trois" "quatre" "cinq" "six" "sept"
  "huit" "neuf" "dix" "cent" "mille" "million" "milliard")

(speech-collection roman french
  "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
  "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

(speech-collection greek french
  "alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota"
  "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma" "tau"
  "upsilon" "phi" "psi" "chi" "omega")

(speech-collection modify-letters french
  "grand" "petit" "majuscule" "minuscule"
  "gras" "droit" "calligraphique" "fraktur" "gothique"
  "tableau noir gras" "sans serif" "machine à écrire")

(speech-collection accept-start french
  "exponentielle" "logarithme" "sinus" "cosinus" "tangente"
  "racine carrée")

(speech-collection accept-end french
  "prime" "factoriel")

(speech-collection reminder-dangerous french
  "a" "à")

(speech-collection dangerous french
  )

(speech-collection dangerous-end french
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General speech commands for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map french std-text
  ("math" (speech-inline 'math))
  ("matt" (speech-inline 'math))
  ("maths" (speech-inline 'math))
  ("formule" (speech-inline 'math))
  ("équation numérotée" (make-equation))
  ("équations numérotées" (make-equation))
  ("grande formule" (make-equation*))
  ("équation" (make-equation*))
  ("équations" (make-equation*))
  ("plusieurs équations" (make-eqnarray*))

  ("abréviation" (speech-inline 'abbr))
  ("description" (make-tmlist 'description))
  ("énumération" (make-tmlist 'enumerate))
  ("liste" (make-tmlist 'itemize))
  ("liste de points" (make-tmlist 'itemize))
  ("liste d'items" (make-tmlist 'itemize))
  ("emphasize" (speech-inline 'em))
  ("nom" (speech-inline 'name))
  ("sample" (speech-inline 'samp))
  ("important" (speech-inline 'strong))
  ("verbatim" (speech-inline 'verbatim))
  ("nouveau point" (make-item))
  ("nouvel item" (make-item))
  ("chapitre" (make-section 'chapter))
  ("section" (make-section 'section))
  ("sous section" (make-section 'subsection))
  ("sous sous section" (make-section 'subsubsection))
  ("paragraphe" (make-section 'paragraph))
  ("sous paragraphe" (make-section 'subparagraph))

  ("preuve" (speech-proof))
  ("démonstration" (speech-proof)))
