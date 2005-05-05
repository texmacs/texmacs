
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mupad.scm
;; DESCRIPTION : Initialize mupad plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mupad-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (mupad-input) mupad)
  (lazy-menu (mupad-menus) mupad-help-menu)
  (menu-extend session-help-icons
  (if (in-mupad?)
      |
      (=> (balloon (icon "tm_help.xpm") "MuPAD documentation")
	  (link mupad-help-menu)))))

(define (mupad-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (string-append (generic-serialize lan t) "\n"))

(plugin-configure mupad
  (:require (url-exists-in-path? "mupad"))
  (:initialize (mupad-initialize))
  (:launch "tm_mupad --texmacs")
  (:serializer ,mupad-serialize)
  (:session "Mupad"))

(texmacs-modes
  (in-mupad-math% #t in-mupad% in-math%)
  (in-mupad-prog% #t in-mupad% in-prog%))

(kbd-map in-mupad-prog?
  ("$"  (insert "$"))
  ("\"" (insert "\""))
  ("."  (insert "."))
;  ("_"  (insert "_"))
  ("`"  (insert "`")))

(kbd-map in-mupad-math?
  ("$"  (insert "$"))
  ("\"" (insert "\""))
  ("."  (insert "."))
  ("`"  (insert "`")))

