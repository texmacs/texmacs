
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrv.scm
;; DESCRIPTION : abbrv style for BibTeX files
;; COPYRIGHT   : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex abbrv)
  (:use (bibtex bib-utils) (bibtex plain)))

(bib-define-style "abbrv" "plain")

(tm-define (bib-format-first-name x)
  (:mode bib-abbrv?)
  (if (bib-null? (list-ref x 1)) ""
      (with f (bib-abbreviate (list-ref x 1) "." `(nbsp))
        (if (bib-name-ends? f ".")
            (tmconcat f '(nbsp))
            (tmconcat f " ")))))
