
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 2b7b32af-fade-4d57-a1d0-836dbec5d0cd
;;
;; MODULE      : init-hide-show.scm
;; DESCRIPTION : Initialize the 'hide-show' plugin
;; COPYRIGHT   : (C) 2004  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (interactive-proc) interactive-proc/callback-wrapper)

(define (hide-show-initialize)
  (lazy-define (hide-show) hide-ia)
  (lazy-define (hide-show) show-ia)
  (lazy-define (hide-show) show-all)
  (menu-extend tools-menu
    ---
    (-> "Hide and show"
	("Hide..." (hide-ia))
	("Show..." (show-ia))
	("Show all" (show-all)))))

(plugin-configure hide-show
  (:require #t)
  (:initialize (hide-show-initialize)))
