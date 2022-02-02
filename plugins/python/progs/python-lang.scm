
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : python-lang.scm
;; DESCRIPTION : Python Language
;; COPYRIGHT   : (C) 2014-2020  François Poulain, Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (python-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "python") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__" "__import__" "abs"
      "all" "any" "apply" "ascii" "basestring" "bin" "bool" "buffer"
      "bytearray" "bytes" "callable" "chr" "classmethod" "cmp" "coerce" "compile"
      "complex" "delattr" "dict" "dir" "divmod" "enumerate" "eval" "execfile"
      "file" "filter" "float" "format" "frozenset" "getattr" "globals" "hasattr"
      "hash" "help" "hex" "id" "input" "int" "intern" "isinstance"
      "issubclass" "iter" "len" "list" "locals" "long" "map" "max"
      "memoryview" "min" "next" "nonlocal" "object" "oct" "open" "ord"
      "pow" "property" "range" "raw_input" "reduce" "reload" "repr" "reversed"
      "round" "set" "setattr" "slice" "sorted" "staticmethod" "str" "sum"
      "super" "tuple" "type" "unichr" "unicode" "vars" "xrange" "zip"
      "BaseException" "Exception" "ArithmeticError" "EnvironmentError" "LookupError"
      "StandardError" "AssertionError" "AttributeError" "BufferError" "EOFError"
      "FloatingPointError" "GeneratorExit" "IOError" "ImportError" "IndentationError"
      "IndexError" "KeyError" "KeyboardInterrupt" "MemoryError" "NameError"
      "NotImplementedError" "OSError" "OverflowError" "ReferenceError" "RuntimeError"
      "StopIteration" "SyntaxError" "SystemError" "SystemExit" "TabError"
      "TypeError" "UnboundLocalError" "UnicodeError" "UnicodeDecodeError" "UnicodeEncodeError"
      "UnicodeTranslateError" "ValueError" "VMSError" "WindowsError" "ZeroDivisionError"
      "BytesWarning" "DeprecationWarning" "FutureWarning" "ImportWarning" "PendingDeprecationWarning"
      "RuntimeWarning" "SyntaxWarning" "UnicodeWarning" "UserWarning" "Warning")
    (declare_function "def" "lambda")
    (declare_module "import")
    (declare_type "class")
    (keyword
      "as" "del" "from" "global" "in" "is" "with")
    (keyword_conditional
      "break" "continue" "elif" "else" "for" "if" "while")
    (keyword_control
      "assert" "except" "exec" "finally" "pass" "print" "raise" "return"
      "try" "yield")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "python") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "and" "not" "or"
      "+" "-" "/" "*" "**" "//" "%" "|" "&" "^"
      "<<" ">>" "==" "!=" "<>" "<" ">" "<=" ">="
      "=" "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^="
      "**=" "//=" "<<=" ">>="
      "~")
   (operator_special ":")
   (operator_decoration "@")
   (operator_field ".")
   (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (python-number-suffix)
  `(suffix
    (imaginary "j" "J")))

;; https://docs.python.org/3.8/reference/lexical_analysis.html#numeric-literals
(tm-define (parser-feature lan key)
  (:require (and (== lan "python") (== key "number")))
  `(,(string->symbol key)
   (bool_features
     "prefix_0x" "prefix_0b" "prefix_0o" "no_suffix_with_box"
     "sci_notation")
   ,(python-number-suffix)
   (separator "_")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "python") (== key "string")))
  `(,(string->symbol key)
    (bool_features 
     "hex_with_8_bits" "hex_with_16_bits"
     "hex_with_32_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v" "newline")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "python") (== key "comment")))
  `(,(string->symbol key)
    (inline "#")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-python-syntax var val)
  (syntax-read-preferences "python"))

(define-preferences
  ("syntax:python:none" "red" notify-python-syntax)
  ("syntax:python:comment" "brown" notify-python-syntax)
  ("syntax:python:error" "dark red" notify-python-syntax)
  ("syntax:python:constant" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_number" "#4040c0" notify-python-syntax)
  ("syntax:python:constant_string" "dark grey" notify-python-syntax)
  ("syntax:python:constant_char" "#333333" notify-python-syntax)
  ("syntax:python:declare_function" "#0000c0" notify-python-syntax)
  ("syntax:python:declare_type" "#0000c0" notify-python-syntax)
  ("syntax:python:operator" "#8b008b" notify-python-syntax)
  ("syntax:python:operator_openclose" "#B02020" notify-python-syntax)
  ("syntax:python:operator_field" "#88888" notify-python-syntax)
  ("syntax:python:operator_special" "orange" notify-python-syntax)
  ("syntax:python:keyword" "#309090" notify-python-syntax)
  ("syntax:python:keyword_conditional" "#309090" notify-python-syntax)
  ("syntax:python:keyword_control" "#309090" notify-python-syntax))
