
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texgraph-scripts-menu.scm
;; DESCRIPTION : routines for on-the-fly evaluation of TeXgraph scripts
;; COPYRIGHT   : Emmanuel Corcelle (corcelle at gmail dot com)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : scripts-menu.scm
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texgraph-scripts-menu)
  (:use (texgraph-scripts-edit)
	(utils plugins plugin-cmd)))

(menu-bind texgraph-scripts-plot-menu
  ("Traceur de courbes" 	
		(init-add-package "texgraph-scripts")
		(delayed
		  (:pause 10) (make 'texgraph-plot-curve)))
;  ("Surfaces" (make 'texgraph-plot-surface))
;  ("Parametric curve" (make 'texgraph-plot-curve*))
;  ("Parametric surface" (make 'texgraph-plot-surface*))
)
