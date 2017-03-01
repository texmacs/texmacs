
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-fricas.scm
;; DESCRIPTION : Initialize fricas plugin
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor - Boston, MA 02110-1335, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure fricas
  (:require (url-exists-in-path? "fricas"))
  (:launch "fricas -texmacs")
  (:session "Fricas")
  (:scripts "Fricas"))

(when (supports-fricas?)
  (import-from (fricas-kbd))
  (import-from (fricas-menus))
  (lazy-input-converter (fricas-input) fricas))
