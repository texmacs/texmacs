
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-clisp.scm
;; DESCRIPTION : Initialize clisp plugin
;; COPYRIGHT   : (C) 2003 Michael Graffam
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure lush
  (:require (url-exists-in-path? "lush"))
  (:launch "tm_lush")
  (:session "Lush"))

(tm-cond-expand (supports-lush?)
  (plugin-input-converters lush))
