;; arch-tag: e6bf1096-dc3b-4fc2-94f5-7f36917aa048

(define (french-punctuation-initialize)
  (define (make-french-right-punctuation str)
    (insert `(concat (hspace "1spc") ,str)))
  (define (make-french-left-punctuation str)
    (insert `(concat ,str (hspace "1spc"))))
  (define (make-french-near-punctuation str)
    (insert `(concat (space "0.25fn") ,str)))

  (kbd-map
    (:mode in-french?)
    (";" (make-french-near-punctuation ";"))
    ("?" (make-french-near-punctuation "?"))
    ("!" (make-french-near-punctuation "!"))
    (":" (make-french-right-punctuation ":"))
    ("< *" (make-french-left-punctuation ""))
    ("> *" (make-french-right-punctuation ""))
    ("< <" (make-french-left-punctuation ""))
    ("> >" (make-french-right-punctuation ""))
    ("e t c ." (insert '(abbr "etc.")))))

(plugin-configure french-punctuation-initialize
  (:require #t)
  (:initialize (french-punctuation-initialize)))
