
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

(define (sage-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/tmpy/session/tm_sage.py")
      (system-url->string "$TEXMACS_PATH/plugins/tmpy/session/tm_sage.py")))

(define (texmacs-cygwin-path)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace (getenv "TEXMACS_HOME_PATH")
           "C:" "/cygdrive/c")
          "\\" "/")
         " " "\\ ")
        "(" "\\(")
       ")" "\\)")
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace (getenv "TEXMACS_PATH")
           "C:" "/cygdrive/c")
          "\\" "/")
         " " "\\ ")
        "(" "\\(")
       ")" "\\)")))

;; TODO: what if there are two different versions of SageMath
(define (sagemath-win-app-url)
  (url-or (url-resolve "C:/Program Files/SageMath*" "r")
          (url-resolve "C:/Program Files (x86)/SageMath*" "r")))

(define (sagemath-bash)
  (string-append (url->system (sagemath-win-app-url)) "\\runtime\\bin\\bash.exe"))

(define (sage-version)
  (string-replace
   (url->system (url-tail (sagemath-win-app-url)))
   "SageMath "
   ""))

(define (sage-launchers)
  (if (os-mingw?)
      `((:launch ,(string-append (raw-quote (sagemath-bash)) " --login -c '/opt/sagemath-" (sage-version) "/sage -python " (texmacs-cygwin-path)  "/plugins/tmpy/session/tm_sage.py'")))
      `((:launch ,(string-append "sage -python " (sage-entry))))))

(define (sage-require)
  (if (os-mingw?)
    (url-exists? (sagemath-win-app-url))
    (url-exists-in-path? "sage")))

(plugin-configure sage
  (:macpath "Sage*" "Contents/Resources/sage")
  (:require (sage-require))
  ,@(sage-launchers)
  (:tab-completion #t)
  (:serializer ,sage-serialize)
  (:session "Sage")
  (:scripts "Sage"))

;(set-session-multiline-input "sage" "default" #t)
;(set-program-multiline-input "sage" "default" #t)

(when (supports-sage?)
  (lazy-input-converter (sage-input) sage))
