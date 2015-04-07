
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-bibtex.scm
;; DESCRIPTION : setup bibtex converters
;; COPYRIGHT   : (C) 2010, 2014  David MICHEL and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert bibtex init-bibtex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw BibTeX, as implemented by David Michel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format bibtex
  (:name "RawBibTeX")
  (:suffix "rawbib")
  (:hidden))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BibTeX, presented using the TeXmacs database format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format tmbib
  (:name "BibTeX")
  (:suffix "bib"))

(lazy-define (database bib-db) tmbib-snippet->texmacs)
(lazy-define (database bib-db) tmbib-document->texmacs)
(lazy-define (database bib-db) texmacs->tmbib-snippet)
(lazy-define (database bib-db) texmacs->tmbib-document)

(converter tmbib-snippet texmacs-stree
  (:function tmbib-snippet->texmacs))

(converter tmbib-document texmacs-stree
  (:function tmbib-document->texmacs))

(converter texmacs-stree tmbib-snippet
  (:function texmacs->tmbib-snippet))

(converter texmacs-stree tmbib-document
  (:function texmacs->tmbib-document))

(define-preferences
  ("bibtex->texmacs:conservative" "on" noop)
  ("texmacs->bibtex:conservative" "on" noop))
