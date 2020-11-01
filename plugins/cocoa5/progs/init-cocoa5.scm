
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-cocoa5.scm
;; DESCRIPTION : Initialize Cocoa plugin
;; COPYRIGHT   : (C) 2014  Francois Poulain
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure cocoa5
  (:winpath "cocoa*" ".")
  (:winpath "Cocoa*" ".")
  (:require (url-exists-in-path? "cocoa5"))
  (:launch "cocoa5 --prompt '\x05\x02verbatim:'")
  (:session "CoCoa5"))
