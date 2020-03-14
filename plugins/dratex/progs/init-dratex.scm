
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-dratex.scm
;; DESCRIPTION : Initialize DraTex plugin
;; COPYRIGHT   : (C) 2005       Nicolas Ratier
;;               (C) 2019-2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (python-command)
  (if (url-exists-in-path? "python3") "python3" "python2"))

(define (python-exists?)
  (or (url-exists-in-path? "python3")
      (url-exists-in-path? "python2")))

(define (dratex-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (dratex-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_dratex.py\"")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_dratex.py\"")))

(plugin-configure dratex
  (:require (python-exists?))
  (:require (url-exists-in-path? "latex"))
  (:launch ,(dratex-launcher))
  (:serializer ,dratex-serialize)
  (:session "DraTex"))
