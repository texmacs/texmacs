
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : dot-lang.scm
;; DESCRIPTION : the DOT Language
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog dot-lang))

(tm-define (dot-keywords)
  (list
   `(declare_type "graph" "node" "edge" "digraph" "subgraph")
   `(keyword
     "color" "rank")))

(tm-define (dot-operators)
  (list
   `(operator
     "," ";" ":" "=")
   `(operator_special
     "-<gtr>" "--")
   `(operator_decoration "@")
   `(operator_field ".")
   `(operator_openclose "{" "[" "(" ")" "]" "}")))
