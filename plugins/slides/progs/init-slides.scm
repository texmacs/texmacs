
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 71f4ca70-083f-493c-b0ff-9d8d577a0d8a
;;
;; MODULE      : init-slides.scm
;; DESCRIPTION : Initialize the 'slides' plugin
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (slides slides) make-slides)
(lazy-define (slides slides) make-slides-here)
(lazy-define (slides browse) next-slide)
(lazy-define (slides browse) prev-slide)
(lazy-define (slides iterate-buffer) clear-output)

(define (slides-initialize)
  (menu-extend insert-fold-menu
    ---
    ("Make slides" (make-slides))
    ("Make slides here" (make-slides-here))
    (when (inside? 'switch)
	  ("Next slide" (next-slide))
	  ("Previous slide" (prev-slide)))
    ---
    ("Clear sessions output" (clear-output)))

  (kbd-map
    ("C-end" (next-slide))
    ("C-home" (prev-slide))))

(plugin-configure slides
  (:require #t)
  (:initialize (slides-initialize)))
