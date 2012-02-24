
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texgraph.scm
;; DESCRIPTION : Initialize texgraph plugin
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : Initialize maxima plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texgraph-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(define (texgraph-initialize)
  (import-from (texgraph-menus))
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (texgraph-input) texgraph) ;; uniquement pour le script plot-curve
  )

(plugin-configure texgraph
  (:require (and (url-exists-in-path? "latex")
		 (url-exists-in-path? "CmdTeXgraph")))
  (:initialize (texgraph-initialize))
  (:launch "tm_texgraph --texmacs")
  (:serializer ,texgraph-serialize)
  (:session "Texgraph")
  (:scripts "Texgraph"))

