
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-matlab.scm
;; DESCRIPTION : Initialize Matlab plugin
;; COPYRIGHT   : (C) 2004 Free Software Foundation
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (matlab-initialize)
  (import-from (utils plugins plugin-convert))
  (plugin-input-converters matlab))

(plugin-configure matlab
  (:require (url-exists-in-path? "matlab"))
  (:initialize (matlab-initialize))
  (:launch "tm_matlab")
  (:session "Matlab"))
