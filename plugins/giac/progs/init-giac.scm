
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-giac.scm
;; DESCRIPTION : Initialize giac plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure giac
  (:macpath "usr" "bin")
  (:require (url-exists-in-path? "giac"))
  (:tab-completion #t)
  (:launch "giac --texmacs")
  (:session "Giac"))

(when (supports-giac?)
  (import-from (giac-menus))
  (lazy-input-converter (giac-input) giac))
