
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-matlab.scm
;; DESCRIPTION : Initialize Matlab plugin
;; COPYRIGHT   : (C) 2004 Free Software Foundation
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (matlab-initialize)
  (import-from (texmacs plugin plugin-convert))
  (plugin-input-converters matlab))

(plugin-configure matlab
  (:require (url-exists-in-path? "matlab"))
  (:initialize (matlab-initialize))
  (:launch "tm_matlab")
  (:session "Matlab"))
