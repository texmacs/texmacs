
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format julia
  (:name "Julia Source Code")
  (:suffix "jl"))
  
(define (texmacs->julia x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (julia->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(define (julia-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x 
    (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree julia-document
  (:function texmacs->julia))

(converter julia-document texmacs-tree
  (:function julia->texmacs))
  
(converter texmacs-tree julia-snippet
  (:function texmacs->julia))

(converter julia-snippet texmacs-tree
  (:function julia-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format java
  (:name "Java Source Code")
  (:suffix "java"))

(define (texmacs->java x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (java->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(define (java-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x 
    (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree java-document
  (:function texmacs->java))

(converter java-document texmacs-tree
  (:function java->texmacs))
  
(converter texmacs-tree java-snippet
  (:function texmacs->java))

(converter java-snippet texmacs-tree
  (:function java-snippet->texmacs))
