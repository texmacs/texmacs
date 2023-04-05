
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : julia-lang.scm
;; DESCRIPTION : Julia Language
;; COPYRIGHT   : (C) 2021 Jeroen Wouters
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://docs.julialang.org/en/v1/base/base/#Keywords
; This is the list of reserved keywords in Julia: baremodule, begin, break, catch, const, 
; continue, do, else, elseif, end, export, false, finally, for, function, global, if, 
; import, let, local, macro, module, quote, return, struct, true, try, using, while.
; Those keywords are not allowed to be used as variable names.
; 
; The following two-word sequences are reserved: abstract type, mutable struct, 
; primitive type. However, you can create variables with names: abstract, mutable,
; primitive and type.
; 
; Finally, where is parsed as an infix operator for writing parametric method and 
; type definitions. Also in and isa are parsed as infix operators. Creation of a 
; variable named where, in or isa is allowed though.


(texmacs-module (julia-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "julia") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "true" "false")
    (declare_function "function" "do")
    (declare_module "import" "using" "module" "baremodule" "export")
    (declare_type "struct" "abstract type" "mutable struct"
      "primitive type")
    (keyword
      "let" "local" "global" "const" "end" "macro" "quote")
    (keyword_conditional
      "break" "continue" "elseif" "else" "for" "if" "while")
    (keyword_control
      "begin" "try" "catch" "return" "finally")))


;; https://docs.julialang.org/en/v1/manual/mathematical-operations/
;; arithm. operators: +, -, *, /, ÷, \, ^, %
;; boolean operators: !, &&, ||
;; bitwise operators: ~, &, |, ⊻, >>>, >>, <<
;; updating operators: +=  -=  *=  /=  \=  ÷=  %=  ^=  &=  |=  ⊻=  >>>=  >>=  <<=
;; dot operators: . before operator
;; numeric comparison: ==, !=, ≠, <, <=, ≤, >, >=, ≥
;; square root: √
;; adjoint (suffix): '

;; https://docs.julialang.org/en/v1/base/punctuation/
;; string and expression interpolation (prefix): $
;; macro (prefix): @
;; subtype operators: <|, |>
;; function composition: ∘
;; splat operator: ...
;; type: ::
;; dictionary pair: =>
;; anonymous function: ->

;; TODO
;; multiline comments (nestable): #=, =#

(tm-define (parser-feature lan key)
  (:require (and (== lan "julia") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "*" "/" "÷" "\\" "^" "%" "!" "&&" "||"
      "~" "&" "|" "⊻" ">>>" ">>" "<<" "+=" "-=" "*="
      "/=" "\\=" "÷=" "%=" "^=" "&=" "|=" "⊻=" ">>>="
      ">>=" "<<=" "==" "!=" "≠" "<" "<=" "≤" ">" ">=" "≥" "√"
      "<|" "|>" "∘" "..." "::" "=>" "->"
      )
   ; (operator_special ":")
    (operator_decoration "@" "$")
   ; (operator_field ".")
   (operator_openclose "{" "[" "(" ")" "]" "}")))

;; https://docs.julialang.org/en/v1/manual/complex-and-rational-numbers/#Rational-Numbers
(define (julia-number-suffix)
   `(suffix
     (imaginary "im")))

;; https://docs.julialang.org/en/v1/manual/integers-and-floating-point-numbers/
(tm-define (parser-feature lan key)
 (:require (and (== lan "julia") (== key "number")))
 `(,(string->symbol key)
  (bool_features
    "prefix_0x" "prefix_0b" "prefix_0o" "no_suffix_with_box"
    "sci_notation")
  ,(julia-number-suffix)
  (separator "_")))
  
  (tm-define (parser-feature lan key)
    (:require (and (== lan "julia") (== key "string")))
    `(,(string->symbol key)
      (bool_features 
       "hex_with_8_bits" "hex_with_16_bits"
       "hex_with_32_bits" "octal_upto_3_digits")
      (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v" "newline")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "julia") (== key "comment")))
  `(,(string->symbol key)
    (inline "#")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-julia-syntax var val)
  (syntax-read-preferences "julia"))

(define-preferences
  ("syntax:julia:none" "red" notify-julia-syntax)
  ("syntax:julia:comment" "brown" notify-julia-syntax)
  ("syntax:julia:error" "dark red" notify-julia-syntax)
  ("syntax:julia:constant" "#4040c0" notify-julia-syntax)
  ("syntax:julia:constant_number" "#4040c0" notify-julia-syntax)
  ("syntax:julia:constant_string" "dark grey" notify-julia-syntax)
  ("syntax:julia:constant_char" "#333333" notify-julia-syntax)
  ("syntax:julia:declare_function" "#0000c0" notify-julia-syntax)
  ("syntax:julia:declare_module" "0000c0" notify-julia-syntax)
  ("syntax:julia:declare_type" "0000c0" notify-julia-syntax)
  ("syntax:julia:operator" "#8b008b" notify-julia-syntax)
  ("syntax:julia:operator_openclose" "#B02020" notify-julia-syntax)
  ("syntax:julia:operator_field" "#88888" notify-julia-syntax)
  ("syntax:julia:operator_special" "orange" notify-julia-syntax)
  ("syntax:julia:keyword" "#309090" notify-julia-syntax)
  ("syntax:julia:keyword_conditional" "#309090" notify-julia-syntax)
  ("syntax:julia:keyword_control" "#309090" notify-julia-syntax))
