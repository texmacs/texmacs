
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-format.scm
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-format)
  (:use (convert rewrite init-rewrite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format cpp
  (:name "C++ source")
  (:suffix "cpp" "cc" "hpp" "hh"))

(define (texmacs->cpp x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (cpp->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(define (cpp-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x 
    (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree cpp-document
  (:function texmacs->cpp))

(converter cpp-document texmacs-tree
  (:function cpp->texmacs))
  
(converter texmacs-tree cpp-snippet
  (:function texmacs->cpp))

(converter cpp-snippet texmacs-tree
  (:function cpp-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format scheme
  (:name "Scheme source")
  (:suffix "scm"))

(define (texmacs->scheme x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (scheme->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(define (scheme-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x 
    (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree scheme-document
  (:function texmacs->scheme))

(converter scheme-document texmacs-tree
  (:function scheme->texmacs))
  
(converter texmacs-tree scheme-snippet
  (:function texmacs->scheme))

(converter scheme-snippet texmacs-tree
  (:function scheme-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format scala
  (:name "Scala source")
  (:suffix "scala"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-format java
  (:name "Java source")
  (:suffix "java"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-format python
  (:name "Python source")
  (:suffix "py"))
