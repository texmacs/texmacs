
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-utils.scm
;; DESCRIPTION : support functions for gui markup in style packages
;; COPYRIGHT   : (C) 2023  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc gui-utils)
  (:use (utils misc tooltip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-on-select type x y cmd)
  (:secure #t)
  ;;(display* "gui-on-select " type ", " x ", " y ", " cmd "\n")
  (or (and (in? type (list "click" "drag")) "done")
      (and (== type "select")
           (begin
             (delayed
               (:idle 1)
               (secure-eval (string->object cmd))
               (close-tooltip))
             "done"))))
