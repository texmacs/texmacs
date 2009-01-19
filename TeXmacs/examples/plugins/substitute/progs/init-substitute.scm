
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-substitute.scm
;; DESCRIPTION : Initialize the 'substitute' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-substitute)
  (import-from (utils plugins plugin-eval))
  (if (selection-active-any?)
      (let* ((t (tree->stree (selection-tree)))
	     (u (plugin-eval "substitute" "default" t)))
	(clipboard-cut "primary")
	(insert (stree->tree u)))))

(kbd-map
  ("C-F12" (substitute-substitute)))

(plugin-configure substitute
  (:require (url-exists-in-path? "substitute.bin"))
  (:launch "substitute.bin")
  (:session "Substitute"))
