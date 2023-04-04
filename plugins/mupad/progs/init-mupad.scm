
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

(define (mupad-serialize lan t)
  (string-append (generic-serialize lan t) "\n"))

(plugin-configure mupad
  (:require (url-exists-in-path? "mupad"))
  (:launch "tm_mupad --texmacs")
  (:serializer ,mupad-serialize)
  (:session "Mupad")
  (:scripts "Mupad"))

(tm-cond-expand (supports-mupad?)
  (import-from (mupad-menus))
  (lazy-input-converter (mupad-input) mupad)

  (texmacs-modes
    (in-mupad-math% #t in-mupad% in-math%)
    (in-mupad-prog% #t in-mupad% in-prog%))

  (kbd-map
    (:mode in-mupad-prog?)
    ("$"  (insert "$"))
    ("\"" (insert "\""))
    ("."  (insert "."))
    ;; ("_"  (insert "_"))
    ("`"  (insert "`")))

  (kbd-map
    (:mode in-mupad-math?)
    ("$"  (insert "$"))
    ("\"" (insert "\""))
    ("."  (insert "."))
    ("`"  (insert "`"))))
