
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-menu.scm
;; DESCRIPTION : routines for on-the-fly evaluation of scripts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-menu)
  (:use (dynamic scripts-edit)))

(menu-bind plugin-eval-menu
  (when (plugin-evaluable?)
	("Evaluate" (plugin-evaluate)))
  ("Evaluation tag" (make 'script-eval))
  ("Evaluation switch" (make-script-input)))

(menu-bind plugin-eval-toggle-menu
  ("Keep evaluated expressions" (toggle-keep-input))
  ("Quick evaluation of formulas" (toggle-eval-math)))
