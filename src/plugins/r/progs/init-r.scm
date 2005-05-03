
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-r.scm
;; DESCRIPTION : Initialize GNU R plugin
;; COPYRIGHT   : (C) 1999  Michael Lachmann and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim 
		      (string-replace s "\n" ";;" 
				      )
		      ) "\n" )))
)

(define (r-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (r-input) r))

(plugin-configure r
  (:require (url-exists-in-path? "R"))
  (:initialize (r-initialize))
  (:serializer ,r-serialize)
  (:launch "exec tm_r")
;;  (:tab-completion #t)
  (:session "R"))


