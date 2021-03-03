
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-octave.scm
;; DESCRIPTION : Initialize octave plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (octave-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (octave-entry)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/octave")
      (system-url->string "$TEXMACS_HOME_PATH/plugins/octave/octave/tmstart.m")
      (system-url->string "$TEXMACS_PATH/plugins/octave/octave/tmstart.m")))

(define (octave-launcher)
  (with boot (raw-quote (octave-entry))
    (if (url-exists-in-path? "octave-cli")
        (string-append "octave-cli -qi " boot)
        (string-append "octave-octave-app -qi " boot))))

; when using `:macpath`, the (octave-launcher) uses `octave-octave-app`
; that's why we are using `plugin-add-macos-path` here
(plugin-add-macos-path "Octave*/Contents/Resources/usr/Cellar/octave-octave-app@*/*" "bin" #t)

(plugin-configure octave
  (:winpath "Octave*" ".")
  (:winpath "Octave*" "bin")
  (:winpath "Octave*" "mingw64/bin")
  (:winpath "Octave/Octave*" ".")
  (:winpath "Octave/Octave*" "bin")
  (:winpath "Octave/Octave*" "mingw64/bin")
  (:macpath "Octave*" "Contents/Resources/usr/bin")
  (:require (or (url-exists-in-path? "octave-cli")
                (url-exists-in-path? "octave-octave-app")))
  (:serializer ,octave-serialize)
  (:launch ,(octave-launcher))
  (:tab-completion #t)
  (:session "Octave"))

(when (supports-octave?)
  (plugin-input-converters octave))
