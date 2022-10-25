
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

(speech-collection prefix french
  "grand" "petit")

(speech-collection postfix french
  "majuscule" "minuscule"
  "gras" "droit" "calligraphique" "fraktur" "gothique"
  "tableau noir gras" "sans serif" "machine à écrire")

(speech-collection prefix french
  "exponentielle" "logarithme" "sinus" "cosinus" "tangente"
  "racine carrée")

(speech-collection postfix french
  "prime" "factoriel")

(speech-collection dangerous french
  "des" "eux" "œuf" "gay" "car" "casse" "j'y" "elle" "aime" "haine"
  "au" "beau" "haut" "os" "paye" "pays" "air" "est-ce"
  "t'es" "tes" "eu" "value" "vert"

  "gamin" "éteins" "est" "état" "mou" "mieux" "mûr" "mus"
  "nue" "nul" "pis" "rock" "rome" "rose" "tôt"
  "taux" "options" "fille" "fit" "qui"

  "assez" "aime"
  "rang" "rend" "irons" "giron" "aileron" "huron" "verrons"
  "ranger" "ronger" "rompez" "remonter"
  "sûr" "assure"
  "bars" "bar" "chapeaux" "utile" "utilité" "tel" "telle"

  "a" "à" "en" "un" "une" "deux" "de" "dans" "the")

(speech-collection skip french
  "et" "ou")

(speech-collection math-mode french
  "math" "maths" "matt" "mathématiques" "formule" "formules")

(speech-collection text-mode french
  "text" "texte")

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
