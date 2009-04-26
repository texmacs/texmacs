
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
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texgraph-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(define (texgraph-initialize)
  (import-from (texgraph-menus))
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (texgraph-input) texgraph)   ;; uniquement pour le script plot-curve
  (menu-extend texmacs-extra-menu
	(if (or (in-texgraph?) (and (not-in-session?) (texgraph-scripts?)))
		(=> "TeXgraph" (link texgraph-functions-menu)))))

(plugin-configure texgraph
  (:require (and (url-exists-in-path? "latex")
		 (url-exists-in-path? "CmdTeXgraph")))
  (:initialize (texgraph-initialize))
  (:launch "tm_texgraph --texmacs")
  (:serializer ,texgraph-serialize)
  (:session "Texgraph")
  (:scripts "Texgraph"))

