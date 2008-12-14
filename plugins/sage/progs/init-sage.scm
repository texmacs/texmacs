
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-sage.scm
;; DESCRIPTION : Initialize SAGE plugin
;; COPYRIGHT   : (C) 2004  Ero Carrera
;; COPYRIGHT   : (C) 2007  Mike Carrera
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure sage
  (:require (url-exists-in-path? "sage"))
  (:require (url-exists-in-path? "tm_sage"))
  (:launch "sage -python `which tm_sage`")
  (:tab-completion #t)
  (:session "Sage"))
