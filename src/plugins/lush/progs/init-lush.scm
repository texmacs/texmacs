
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-clisp.scm
;; DESCRIPTION : Initialize clisp plugin
;; COPYRIGHT   : (C) 2003 Michael Graffam
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lush-initialize)
  (import-from (texmacs plugin plugin-convert))
  (plugin-input-converters lush))

(plugin-configure lush
  (:require (url-exists-in-path? "lush"))
  (:initialize (lush-initialize))
  (:launch "tm_lush")
  (:session "Lush"))
