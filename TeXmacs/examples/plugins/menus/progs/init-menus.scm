
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-menus.scm
;; DESCRIPTION : Initialize the 'menus' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind menus-menu
  ("Hi" (insert-string "Hello world")))

(menu-extend texmacs-extra-menu
  (if (equal? (get-env "prog language") "menus")
      (=> "Menus" (link menus-menu))))

(define-macro (menus-add s)
  `(menu-extend menus-menu
     (,s (insert-string ,s))))

(plugin-configure menus
  (:require (url-exists-in-path? "menus.bin"))
  (:launch "menus.bin")
  (:session "Menus"))
