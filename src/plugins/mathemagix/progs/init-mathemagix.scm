
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mathemagix.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathemagix-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (mathemagix-input) mathemagix))

(texmacs-modes
  (in-mathemagix-math% #t in-mathemagix% in-math%))

(kbd-map
  (:mode in-mathemagix-math?)
  ;;("'" "'")
  ("\"" "\""))

(if (url-exists-in-path? "mmx-light")
    (plugin-configure mathemagix
      (:require (url-exists-in-path? "mmx-light"))
      (:initialize (mathemagix-initialize))
      (:launch "mmx-light --texmacs")
      (:session "Mathemagix"))
    (plugin-configure mathemagix
      (:require (url-exists-in-path? "mmx-shell"))
      (:initialize (mathemagix-initialize))
      (:launch "mmx-shell --texmacs")
      (:session "Mathemagix")))
