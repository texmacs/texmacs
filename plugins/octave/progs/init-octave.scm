
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

(define octave-launcher
  (string-append "octave-cli -qi " (octave-source-path) "/tm-start.m"))

(plugin-configure octave
  (:winpath "Octave*" "bin")
  (:require (url-exists-in-path? "octave"))
  (:launch ,octave-launcher)
  (:session "Octave"))

(when (supports-octave?)
  (plugin-input-converters octave))
