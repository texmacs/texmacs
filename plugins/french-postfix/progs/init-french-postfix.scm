;; arch-tag: 2f3ac941-db83-41db-beb2-92476e483697

(define (french-postfix-initialize)
  (kbd-map
    (:mode in-french?)
    ("a `" "à") ("A `" "À") ("a ^" "â") ("A ^" "Â")
    ("e '" "é") ("E '" "É") ("e `" "è") ("E `" "È")
    ("e ^" "ê") ("E ^" "Ê") ("e \"" "ë") ("E \"" "Ë")
    ("e ' '" "e'") ("E ' '" "E'") ("e \" \"" "e\"") ("E \" \"" "E\"")
    ("i ^" "î") ("I ^" "Î") ("i \"" "ï") ("I \"" "Ï")
    ("i \" \"" "i\"") ("I \" \"" "I\"")
    ("o ^" "ô") ("O ^" "Ô")
    ("u `" "ù") ("U `" "Ù") ("u ^" "û") ("U ^" "Û")
    ("c ," "ç") ("C ," "Ç") ("c , ," "c,") ("C , ," "C,")
    ("a e" "æ") ("A e" "Æ") ("A E" "Æ")
    ("a e e" "ae") ("A e e" "Ae") ("A E E" "AE")
    ("o e" "÷") ("O e" "×") ("O E" "×")
    ("o e e" "oe") ("O e e" "Oe") ("O E E" "OE")))

(plugin-configure french-postfix
  (:require #t)
  (:initialize (french-postfix-initialize)))
