
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-substitute.scm
;; DESCRIPTION : Initialize the 'substitute' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-substitute)
  (import-from (utils plugins plugin-cmd))
  (if (selection-active-any?)
      (let* ((t (tree->stree (the-selection)))
	     (u (plugin-eval "substitute" "default" t)))
	(clipboard-cut "primary")
	(insert (stree->tree u)))))

(kbd-map
  ("C-F12" (substitute-substitute)))

(plugin-configure substitute
  (:require (url-exists-in-path? "substitute.bin"))
  (:launch "substitute.bin")
  (:session "Substitute"))
