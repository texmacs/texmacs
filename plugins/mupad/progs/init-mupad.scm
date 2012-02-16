
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mupad.scm
;; DESCRIPTION : Initialize mupad plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mupad-initialize)
  (import-from (utils plugins plugin-convert))
  (import-from (mupad-menus))
  (lazy-input-converter (mupad-input) mupad))

(define (mupad-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (string-append (generic-serialize lan t) "\n"))

(plugin-configure mupad
  (:require (url-exists-in-path? "mupad"))
  (:initialize (mupad-initialize))
  (:launch "tm_mupad --texmacs")
  (:serializer ,mupad-serialize)
  (:session "Mupad")
  (:scripts "Mupad"))

(texmacs-modes
  (in-mupad-math% #t in-mupad% in-math%)
  (in-mupad-prog% #t in-mupad% in-prog%))

(kbd-map
  (:mode in-mupad-prog?)
  ("$"  (insert "$"))
  ("\"" (insert "\""))
  ("."  (insert "."))
;  ("_"  (insert "_"))
  ("`"  (insert "`")))

(kbd-map
  (:mode in-mupad-math?)
  ("$"  (insert "$"))
  ("\"" (insert "\""))
  ("."  (insert "."))
  ("`"  (insert "`")))
