
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format cpp
  (:name "C++ Source Code")
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
