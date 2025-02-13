
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : caas-format.scm
;; DESCRIPTION : Mathemagix file format
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (caas-format)
  (:use (mathemagix-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caas source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format caas
  (:name "Caas source code"))

(tm-define (texmacs->caas x . opts)
  (apply texmacs->mathemagix (cons x opts)))

(tm-define (caas->texmacs x . opts)
  (apply mathemagix->texmacs (cons x opts)))

(tm-define (caas-snippet->texmacs x . opts)
  (apply mathemagix-snippet->texmacs (cons x opts)))

(converter texmacs-tree caas-document
  (:function texmacs->mathemagix))

(converter caas-document texmacs-tree
  (:function mathemagix->texmacs))
  
(converter texmacs-tree caas-snippet
  (:function texmacs->mathemagix))

(converter caas-snippet texmacs-tree
  (:function mathemagix-snippet->texmacs))
