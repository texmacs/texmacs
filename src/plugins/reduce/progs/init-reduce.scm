
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-reduce.scm
;; DESCRIPTION : Initialize reduce plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reduce-initialize)
  (import-from (texmacs plugin plugin-convert))
  (lazy-input-converter (reduce-input) reduce)
  (lazy-menu (reduce-menus) reduce-help-menu)
  (menu-extend texmacs-session-help-icons
  (if (and (in-reduce?) (url-exists? "$reduce/doc/manual/abstract.tex"))
      |
      (=> (balloon (icon "tm_help.xpm") "Reduce documentation")
	  (link reduce-help-menu)))))

(plugin-configure reduce
  (:require (url-exists-in-path? "reduce"))
  (:initialize (reduce-initialize))
  (:launch "tm_reduce")
  (:session "Reduce"))
