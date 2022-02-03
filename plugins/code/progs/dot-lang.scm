
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

(texmacs-module (dot-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "dot") (== key "keyword")))
  `(,(string->symbol key)
    (declare_type "graph" "node" "edge" "digraph" "subgraph")
    (keyword
      ;; Graph attributes
      "center" "layers" "margin" "mclimit" "name" "nodesep" "nslimit"
      "ordering" "page" "pagedir" "rank" "rankdir" "ranksep" "ratio"
      "rotate" "size"
      ;; Node attributes
      "distortion" "fillcolor" "height" "layer" "orientation" "penwidth"
      "peripheries" "regular" "shape" "shapefile" "sides" "skew" "width"
      ;; Edge attributes
      "arrowhead" "arrowsize" "arrowtail" "constraint" "decorateP" "dir"
      "headclip" "headlabel" "labelangel" "labeldistance" "labelfontcolor"
      "labelfontname" "labelfontsize" "labeljust" "labeltooltip" "minlen"
      "port_label_distance" "samehead" "sametail" "tailclip" "taillabel"
      "weight"
      ;; Shared attributes (graphs, nodes, edges)
      "color"
      ;; Shared attributes (graphs and edges)
      "bgcolor" "label" "URL"
      ;; Shared attributes (graphs and edges)
      "fontcolor" "fontname" "fontsize" "layer" "style")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "dot") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "," ";" ":" "=")
    (operator_special
      "->" "--")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(define (dot-number-suffix)
  `(suffix
    (long "l" "L")
    (double "d" "D")
    (float "f" "F")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "dot") (== key "number")))
  `(,(string->symbol key)
    (bool_features
     "prefix_0x" "prefix_0b"
     "sci_notation")
    ,(dot-number-suffix)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "dot") (== key "string")))
  `(,(string->symbol key)
    (bool_features)
    (escape_sequences "\\" "\"" "'" "b" "f" "n" "r" "t")))
