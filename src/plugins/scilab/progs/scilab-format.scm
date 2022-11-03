
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scilab-format.scm
;; DESCRIPTION : Scilab file format
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (scilab-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scilab source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format scilab
  (:name "Scilab source code")
  (:suffix "sce" "sci"))

(define (texmacs->scilab x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (scilab->texmacs x . opts)
  (code->texmacs x))

(define (scilab-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree scilab-document
  (:function texmacs->scilab))

(converter scilab-document texmacs-tree
  (:function scilab->texmacs))
  
(converter texmacs-tree scilab-snippet
  (:function texmacs->scilab))

(converter scilab-snippet texmacs-tree
  (:function scilab-snippet->texmacs))
