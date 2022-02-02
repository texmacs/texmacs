
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format scala
  (:name "Scala Source Code")
  (:suffix "scala"))

(define (texmacs->scala x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (scala->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(define (scala-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x 
    (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter texmacs-tree scala-document
  (:function texmacs->scala))

(converter scala-document texmacs-tree
  (:function scala->texmacs))
  
(converter texmacs-tree scala-snippet
  (:function texmacs->scala))

(converter scala-snippet texmacs-tree
  (:function scala-snippet->texmacs))
