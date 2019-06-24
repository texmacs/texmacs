
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

(define (sage-launchers)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/sage")
      `((:launch ,(string-append "sage -python "
                                 (getenv "TEXMACS_HOME_PATH")
                                 "/plugins/sage/python/bridge.py")))
      `((:launch ,(string-append "sage -python "
                                 (getenv "TEXMACS_PATH")
                                 "/plugins/sage/python/bridge.py")))))

(plugin-configure sage
  (:macpath "Sage*" "Contents/Resources/sage")
  (:require (url-exists-in-path? "sage"))
  ,@(sage-launchers)
  (:tab-completion #t)
  (:session "Sage")
  (:scripts "Sage"))

(when (supports-sage?)
  (lazy-input-converter (sage-input) sage))
