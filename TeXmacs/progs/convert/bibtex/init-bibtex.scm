
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-bibtex.scm
;; DESCRIPTION : setup bibtex converters
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert bibtex init-bibtex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BibTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format bibtex
  (:name "BibTeX")
  (:suffix "bib"))

(lazy-define (convert bibtex bibtextm) parse-bibtex-snippet)
(lazy-define (convert bibtex bibtextm) parse-bibtex-document)
(lazy-define (convert bibtex bibtextm) bibtex->texmacs)
(lazy-define (convert bibtex bibtexout) serialize-bibtex)
(lazy-define (convert bibtex tmbibtex) texmacs->bibtex)

(converter bibtex-snippet bibtex-stree
  (:function parse-bibtex-snippet))

(converter bibtex-document bibtex-stree
  (:function parse-bibtex-document))

(converter bibtex-stree texmacs-stree
  (:function bibtex->texmacs))

(converter bibtex-stree bibtex-document
  (:function serialize-bibtex))

(converter bibtex-stree bibtex-snippet
  (:function serialize-bibtex))

(converter texmacs-stree bibtex-stree
  (:function texmacs->bibtex))

