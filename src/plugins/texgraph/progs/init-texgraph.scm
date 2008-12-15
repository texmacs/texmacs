
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texgraph.scm
;; DESCRIPTION : Initialize texgraph plugin
;; BY	       : Emmanuel Corcelle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : Initialze maxima plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-menu (texgraph-menus) texgraph-functions-menu)

(define (texgraph-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (texgraph-input) texgraph)
  (menu-extend texmacs-extra-menu
    (if (in-texgraph?)
	(=> "TeXgraph" (link texgraph-functions-menu)))))

(plugin-configure texgraph
  (:require (url-exists-in-path? "latex"))
  (:initialize (texgraph-initialize))
;  (:tab-completion #t)
  (:launch "tm_texgraph --texmacs")
  (:session "Texgraph"))

