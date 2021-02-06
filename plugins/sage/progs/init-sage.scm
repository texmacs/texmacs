
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-sage.scm
;; DESCRIPTION : Initialize SageMath plugin
;; COPYRIGHT   : (C) 2004  Ero Carrera
;; COPYRIGHT   : (C) 2007  Mike Carrera
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-modules (dynamic session-edit) (dynamic program-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically, the serializer makes the input preserve the newlines
;; and adds the string character "\n<EOF>\n" by the end.
;; I guess it could send "\x04" instead to signal a real EOF,
;; but I would need to check if that does not kill the pipe...
;; An alternative approach is to use the input-done? command
;; from TeXmacs, but, at the time of this writing, it did not work.--A

(define (sage-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u) "SourceCode")
      (string-append  s  "\n<EOF>\n"))))

(define (sage-launchers)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      `((:launch ,(string-append "sage -python "
                                 (getenv "TEXMACS_HOME_PATH")
                                 "/plugins/tmpy/session/tm_sage.py")))
      `((:launch ,(string-append "sage -python "
                                 (getenv "TEXMACS_PATH")
                                 "/plugins/tmpy/session/tm_sage.py")))))

(plugin-configure sage
  (:winpath "Sage*" "bin")
  (:winpath "Sage*/runtime" "bin")
  (:macpath "Sage*" "Contents/Resources/sage")
  (:require (url-exists-in-path? "sage"))
  ,@(sage-launchers)
  (:tab-completion #t)
  (:serializer ,sage-serialize)
  (:session "Sage")
  (:scripts "Sage"))

;(set-session-multiline-input "sage" "default" #t)
;(set-program-multiline-input "sage" "default" #t)

(when (supports-sage?)
  (lazy-input-converter (sage-input) sage))
