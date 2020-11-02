
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-python.scm
;; DESCRIPTION : Initialize python plugin
;; COPYRIGHT   : (C) 2004  Ero Carrera,
;;               (C) 2012  Adrian Soto
;;               (C) 2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (dynamic session-edit) (dynamic program-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically, the serializer makes the input preserve the newlines
;; and adds the string character "\n<EOF>\n" by the end.
;; I guess it could send "\x04" instead to signal a real EOF,
;; but I would need to check if that does not kill the pipe...
;; An alternative approach is to use the input-done? command
;; from TeXmacs, but, at the time of this writing, it did not work.--A

(define (python-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append  s  "\n<EOF>\n"))))

(define (python-command)
  (if (url-exists-in-path? "python3")
      "python3"
      "python"))

(define (python-exists?)
  (or (url-exists-in-path? "python3")
      (url-exists-in-path? "python")))

(define (python-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_python.py\"")))

(plugin-configure python
  (:winpath "python*" ".")
  (:winpath "Python*" ".")
  (:winpath "Python/Python*" ".")
  (:require (python-exists?))
  (:launch ,(python-launcher))
  (:tab-completion #t)
  (:serializer ,python-serialize)
  (:session "Python")
  (:scripts "Python"))

;(set-session-multiline-input "python" "default" #t)
;(set-program-multiline-input "python" "default" #t)

(when (supports-python?)
  (import-from (python-widgets) (python-menus)))
