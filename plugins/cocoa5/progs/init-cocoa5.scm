
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-cocoa5.scm
;; DESCRIPTION : Initialize Cocoa plugin
;; COPYRIGHT   : (C) 2014, 2020  Francois Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cocoa5-launcher)
  (if (url-exists-in-path? "cocoa5")
      "cocoa5 --prompt '\x05\x02verbatim:'"
      "CoCoAInterpreter --prompt '\x05\x02verbatim:'"))

(plugin-configure cocoa5
  (:winpath "cocoa*" ".")
  (:winpath "Cocoa*" ".")
  (:require (or (url-exists-in-path? "cocoa5")
                (url-exists-in-path? "CoCoAInterpreter")))
  (:launch ,(cocoa5-launcher))
  (:session "CoCoa5"))
