
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
  ;; latin letters
  "sait" "des" "eux" "œuf" "gay" "âge" "hache" "il" "ils"
  "j'y" "car" "cas" "casse" "aile" "ailes" "elle" "aime"
  "au" "beau" "eau" "eaux" "haut" "os" "paye" "pays" "air" "est-ce"
  "t'es" "taille" "tes" "eu" "vais" "value" "vert"

  ;; greek letters
  "bâtard" "gamin" "éteins" "est" "atteint" "état" "tata"
  "mou" "mieux" "mur" "mûr" "mus" "nue" "nul" "pile" "pis"
  "euro" "euros" "robe" "robot" "rock" "rome" "rose" "rouge"
  "auto" "tôt" "taux" "options" "fille" "fit" "qui"

  ;; letter combinations
  "assez" "ai" "août" "avez" "bébé" "baisser"
  "déesse" "gaité" "dévérrouiller" "acheter" "achevé"
  "je"

  ;; punctuation
  "tel" "telle"

  ;; operators 'plus', 'moins', 'fois'
  "opus" "pupuce" "moi" "monter" "noisette" "manteau"
  "foie" "fort" "photo" "photos"

  ;; composition 'rond'
  "rang" "rend" "irons" "giron" "caron" "aileron" "huron" "verrons"
  "ranger" "ronger" "rompez" "remonter"

  ;; predicates 'égal'
  "égalité" "également"
  
  ;; operators and function application
  "dette" "bédé" "idée" "rodé" "décès"
 
  ;; fractions
  "sûr" "assure" "culture" "mesure" "chaussure" "chaussures"
  "surgé" "surveille" "sureau" "surtout"

  ;; wide accents
  "chapeaux" "utile" "utilité" "bars" "bar"

  ;; particularly dangerous adjustments
  "a" "à" "ai" "en" "un" "une" "si" "deux" "de" "dans" "the"
  "le" "la" "ne")

(speech-collection skip french
  "et" "ma" "ou")

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
