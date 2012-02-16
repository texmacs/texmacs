
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-menus.scm
;; DESCRIPTION : Initialize the 'menus' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-items '("Hi"))

(tm-menu (menus-menu)
  (for (entry menu-items)
    ((eval entry) (insert entry))))

(tm-define (menus-add entry)
  (set! menu-items (cons entry menu-items)))

(plugin-configure menus
  (:require (url-exists-in-path? "menus.bin"))
  (:launch "menus.bin")
  (:session "Menus"))

(menu-bind plugin-menu
  (:require (in-menus?))
  (=> "Menus" (link menus-menu)))
