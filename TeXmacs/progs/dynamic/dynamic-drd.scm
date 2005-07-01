
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : dynamic-drd.scm
;; DESCRIPTION : data relation definitions for dynamic tags
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic dynamic-drd)
  (:use (utils edit variants)))

;; fold <-> unfold toggles

(define-group toggle-tag (fold-tag))
(define-group toggle-first-tag (folded-tag))
(define-group toggle-second-tag (unfolded-tag))
(define-group variant-tag (folded-tag) (unfolded-tag))
(define-group similar-tag (fold-tag))

(tm-define toggle-table (make-ahash-table))
(tm-define-macro (define-fold folded unfolded)
  `(begin
     (define-group folded-tag ,folded)
     (define-group unfolded-tag ,unfolded)
     (define-group fold-tag ,folded ,unfolded)
     (ahash-set! toggle-table ',folded ',unfolded)
     (ahash-set! toggle-table ',unfolded ',folded)))

(define-fold fold unfold)
(define-fold fold-plain unfold-plain)
(define-fold fold-std unfold-std)
(define-fold fold-env unfold-env)
(define-fold fold-bracket unfold-bracket)

;; condensed <-> detailed toggles

(define-group toggle-tag (condense-tag))
(define-group toggle-first-tag (condensed-tag))
(define-group toggle-second-tag (detailed-tag))
(define-group variant-tag (condensed-tag) (detailed-tag))
(define-group similar-tag (condense-tag))

(tm-define-macro (define-condense short long)
  `(begin
     (define-group condensed-tag ,short)
     (define-group detailed-tag ,long)
     (define-group condense-tag ,short ,long)
     (ahash-set! toggle-table ',short ',long)
     (ahash-set! toggle-table ',long ',short)))

(define-condense condensed detailed)
(define-condense condensed-algorithm detailed-algorithm)

;; switches

(define-group switch-tag
  (alternative-tag) (unroll-tag) (expanded-tag))

(define-group big-switch-tag
  switch unroll expanded)

(define-group alternative-tag
  switch)

(define-group unroll-tag
  unroll)

(define-group expanded-tag
  expanded)
