
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-giac.scm
;; DESCRIPTION : Initialize giac plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (giac-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (giac-input) giac)
  (lazy-menu (giac-menus) giac-functions-menu)
  (menu-extend texmacs-extra-menu
    (if (in-giac?)
	(=> "Giac" (link giac-functions-menu)))))

(plugin-configure giac
  (:require (url-exists-in-path? "giac"))
  (:initialize (giac-initialize))
  (:tab-completion #t)
  (:launch "giac --texmacs")
  (:session "Giac"))
