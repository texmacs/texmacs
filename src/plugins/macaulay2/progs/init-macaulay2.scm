
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-macaulay2.scm
;; DESCRIPTION : Initialize Macaulay 2 plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macaulay2-initialize)
  (import-from (doc help-funcs) (utils plugins plugin-convert))
  (lazy-input-converter (m2-input) macaulay2)
  (menu-extend texmacs-session-help-icons
    (if (and (in-macaulay2?) (url-exists? "$M2HOME/html/index.html"))
	|
	((balloon (icon "tm_help.xpm") "Macaulay2 manual")
	 (load-help-buffer "$M2HOME/html/index.html")))))

(plugin-configure macaulay2
  (:require (url-exists-in-path? "M2"))
  (:initialize (macaulay2-initialize))
  (:launch "M2 --texmacs")
  (:session "Macaulay 2"))
