
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

(define (octave-source-path)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/octave")
      (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/octave/octave")
      (string-append (getenv "TEXMACS_PATH") "/plugins/octave/octave")))

(define (octave-launcher)
  (with boot (string-append "\"" (octave-source-path) "/tmstart.m\"")
    (if (url-exists-in-path? "octave-cli")
        (string-append "octave-cli -qi " boot)
        (string-append "octave-octave-app -qi " boot))))

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
  (:launch ,(octave-launcher))
  (:session "Octave"))

(when (supports-octave?)
  (plugin-input-converters octave))
