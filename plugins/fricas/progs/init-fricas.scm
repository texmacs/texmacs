
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-axiom.scm
;; DESCRIPTION : Initialize axiom plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (fricas-initialize)
;  (import-from (utils plugins plugin-convert))
;  (lazy-input-converter (fricas-input) fricas))

(plugin-configure fricas
  (:require (url-exists-in-path? "fricas"))
  ;(:initialize (fricas-initialize))
  (:launch "fricas -texmacs")
  (:session "Fricas"))
