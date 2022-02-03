
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : json-lang.scm
;; DESCRIPTION : JSON Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (json-lang))

(tm-define (parser-feature lan key)
  (:require (and (== lan "json") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "false" "true" "null")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-update-expressions
(tm-define (parser-feature lan key)
  (:require (and (== lan "json") (== key "operator")))
  `(,(string->symbol key)
    (operator "+" "-" ":" ",")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

;; Ref: https://ecma-international.org/ecma-262/10.0/index.html#sec-literals-numeric-literals
(tm-define (parser-feature lan key)
  (:require (and (== lan "javascript") (== key "number")))
  `(,(string->symbol key)
    (bool_features
      "sci_notation")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "json") (== key "string")))
  `(,(string->symbol key)
    (bool_features )
    (escape_sequences "\\" "/" "\"" "b" "f" "n" "r" "t")))
