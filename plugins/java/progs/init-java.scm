
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
