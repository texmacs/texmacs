
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pari.scm
;; DESCRIPTION : Initialize python plugin
;; COPYRIGHT   : (C) 2004  Ero Carrera,
;;               (C) 2012  Adrian Soto
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Basically, the serializer makes the input preserve the newlines
;;and adds the string character "\n<EOF>\n" by the end.
;;I guess it could send "\x04" instead to signal a real EOF,
;;but I would need to check if that does not kill the pipe...
;;An alternative approach is to use the input-done? command
;;from TeXmacs, but, at the time of this writing, it did not work.--A

(define (python-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append  s  "\n<EOF>\n"))))


(plugin-configure python
  (:require (url-exists-in-path? "python"))
  (:require (url-exists-in-path? "tm_python"))
  (:launch "tm_python --texmacs")
  (:tab-completion #t)
  (:serializer ,python-serialize)
  (:session "Python"))
