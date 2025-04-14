
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathemagix-format.scm
;; DESCRIPTION : Mathemagix file format
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mathemagix-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathemagix source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format mathemagix
  (:name "Mathemagix source code")
  (:suffix "mmx" "mmh"))

(tm-define (texmacs->mathemagix x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(tm-define (mathemagix->texmacs x . opts)
  (code->texmacs x))

(tm-define (mathemagix-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree mathemagix-document
  (:function texmacs->mathemagix))

(converter mathemagix-document texmacs-tree
  (:function mathemagix->texmacs))
  
(converter texmacs-tree mathemagix-snippet
  (:function texmacs->mathemagix))

(converter mathemagix-snippet texmacs-tree
  (:function mathemagix-snippet->texmacs))
