
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-fricas.scm
;; DESCRIPTION : Initialize fricas plugin
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure fricas
  (:require (url-exists-in-path? "fricas"))
  (:launch "fricas -texmacs")
  (:session "FriCAS")
  (:scripts "FriCAS"))

(when (supports-fricas?)
  (import-from (fricas-kbd))
  (import-from (fricas-menus))
  (lazy-input-converter (fricas-input) fricas))
