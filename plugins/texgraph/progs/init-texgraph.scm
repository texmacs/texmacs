
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
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(plugin-configure texgraph
  (:macpath "TeXgraph.app" "Contents/Resources/TeXgraph")
  (:require (and (url-exists-in-path? "latex")
		 (or (url-exists-in-path? "CmdTeXgraph")
                     (url-exists-in-path? "CmdTeXgraph.sh"))))
  (:launch "tm_texgraph --texmacs")
  (:serializer ,texgraph-serialize)
  (:session "Texgraph")
  (:scripts "Texgraph"))

(tm-cond-expand (supports-texgraph?)
  (import-from (texgraph-menus))
  (lazy-input-converter (texgraph-input) texgraph) ;; uniquement pour le script plot-curve
  )
