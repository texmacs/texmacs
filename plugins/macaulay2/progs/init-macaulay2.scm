
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-macaulay2.scm
;; DESCRIPTION : Initialize Macaulay2 plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure macaulay2
  (:macpath "Macaulay2*" "bin")
  (:require (url-exists-in-path? "M2"))
  (:launch "M2 --texmacs")
  (:session "Macaulay2"))

(when (supports-macaulay2?)
  (import-from (doc help-funcs))
  (lazy-input-converter (m2-input) macaulay2)

  (menu-bind session-help-icons
    (:require (and (in-macaulay2?) (url-exists? "$M2HOME/html/index.html")))
    /
    ((balloon (icon "tm_help.xpm") "Macaulay2 manual")
     (load-help-buffer "$M2HOME/html/index.html"))))
