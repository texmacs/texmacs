
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : config-kbd.scm
;; DESCRIPTION : keyboard configuration
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard config-kbd)
  (:use (texmacs texmacs tm-server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bypassing the pre-edit mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (disable-pre-edit? key) #f)
(tm-define (downgrade-pre-edit key) "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cyrillic input method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-cyrillic-input-method var val)
  (cond
   ((== val "translit")
    (lazy-keyboard (text cyrillic translit-kbd) in-cyrillic-translit?))
   ((== val "jcuken")
    (lazy-keyboard (text cyrillic jcuken-kbd) in-cyrillic-jcuken?))
   ((== val "yawerty")
    (lazy-keyboard (text cyrillic yawerty-kbd) in-cyrillic-yawerty?))
   ((== val "koi8-r")
    (lazy-keyboard (text cyrillic koi8-kbd) in-cyrillic-koi8?))
   ((== val "cp1251")
    (lazy-keyboard (text cyrillic cp1251-kbd) in-cyrillic-cp1251?))))

(define-preferences
  ("cyrillic input method" "none" notify-cyrillic-input-method))
