
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bibtextm.scm
;; DESCRIPTION : conversion of bibtex trees to TeXmacs trees
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert bibtex bibtextm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-bibtex s)
  (tree->stree (parse-bib s)))

(tm-define (parse-bibtex-snippet s)
  (parse-bibtex s))

(tm-define (parse-bibtex-document s)
  `(!file (document
	    (style "bibliography")
	    (body ,(parse-bibtex s)))))

(tm-define (bibtex->texmacs bib)
  (:type (-> stree stree))
  (:synopsis "Convert a parsed BibTeX stree @t into a TeXmacs stree.")
  (let* ((snippet? (not (func? bib '!file 1)))
	 (body (if snippet? bib (cadr bib))))
    body))

