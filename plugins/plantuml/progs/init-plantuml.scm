
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-plantuml.scm
;; DESCRIPTION : Initialize PlantUML plugin
;; COPYRIGHT   : (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (dynamic session-edit) (dynamic program-edit))

(define (plantuml-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (python-command)
  (if (url-exists-in-path? "python3") "python3" "python2"))

(define (python-exists?)
  (or (url-exists-in-path? "python3")
      (url-exists-in-path? "python2")))

(define (plantuml-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command)
                     " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_plantuml.py\"")
      (string-append (python-command)
                     " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_plantuml.py\"")))

(plugin-configure plantuml
  (:require (url-exists-in-path? "plantuml"))
  (:require (python-exists?))
  (:launch ,(plantuml-launcher))
  (:serializer ,plantuml-serialize)
  (:session "PlantUML"))

