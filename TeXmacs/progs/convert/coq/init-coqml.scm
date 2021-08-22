
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-coqml.scm
;; DESCRIPTION : Setup Coq ML converters
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq init-coqml)
    (:use (convert coq gallinatm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gallina
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format gallina
  (:name "Gallina")
  (:hidden))

(lazy-define (convert coq gallinatm)     parse-gallina-snippet)
(lazy-define (convert coq gallinatm)     parse-gallina-document)
(lazy-define (convert coq gallinatm)     gallina->texmacs)
(lazy-define (convert coq tmgallina)     texmacs->gallina)

(converter gallina-document gallina-stree
  (:function parse-gallina-document))

;(converter gallina-stree gallina-document
;  (:function serialize-gallina))

(converter gallina-snippet gallina-stree
  (:function parse-gallina-snippet))

;(converter gallina-stree gallina-snippet
;  (:function serialize-gallina))

(converter gallina-stree texmacs-stree
  (:function gallina->texmacs))

(converter texmacs-stree gallina-stree
  (:function texmacs->gallina))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vernacular
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format vernac
  (:name "Coq Vernacular")
  (:suffix "v"))

(lazy-define (convert coq tmvernac) texmacs->vernac)
(lazy-define (convert coq tmvernac) texmacs->vernac-document)

(converter vernac-snippet texmacs-tree
  (:function vernac->texmacs))

(converter vernac-document texmacs-tree
  (:function vernac-document->texmacs))

(converter texmacs-tree vernac-snippet
  (:function texmacs->vernac))

(converter texmacs-tree vernac-document
  (:function texmacs->vernac-document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoqML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format coqtopml
  (:name "CoqTopML")
  (:suffix "coqtopml")
  (:hidden))

(lazy-define (convert coqml coqtopmlout) serialize-coqtopml)
(lazy-define (convert coqml scmcoqtopml) stree->coqtopml)
(lazy-define (convert coqml coqtopmlscm) coqtopml->stree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (convert coq coqmltest) test-coqml)
