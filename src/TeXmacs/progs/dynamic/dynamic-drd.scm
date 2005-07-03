
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

(define-group toggle-tag (folded-tag) (unfolded-tag))
(define-group toggle-first-tag (folded-tag))
(define-group toggle-second-tag (unfolded-tag))
(define-group variant-tag (folded-tag) (unfolded-tag))
(define-group similar-tag (folded-tag) (unfolded-tag))

(tm-define toggle-table (make-ahash-table))
(tm-define-macro (define-fold folded unfolded)
  `(begin
     (define-group folded-tag ,folded)
     (define-group unfolded-tag ,unfolded)
     (ahash-set! toggle-table ',folded ',unfolded)
     (ahash-set! toggle-table ',unfolded ',folded)))

(define-fold folded unfolded)
(define-fold folded-plain unfolded-plain)
(define-fold folded-std unfolded-std)
(define-fold folded-env unfolded-env)
(define-fold folded-grouped unfolded-grouped)

;; summarized <-> detailed toggles

(define-group toggle-tag (summarized-tag) (detailed-tag))
(define-group toggle-first-tag (summarized-tag))
(define-group toggle-second-tag (detailed-tag))
(define-group variant-tag (summarized-tag) (detailed-tag))
(define-group similar-tag (summarized-tag) (detailed-tag))

(tm-define-macro (define-summarize short long)
  `(begin
     (define-group summarized-tag ,short)
     (define-group detailed-tag ,long)
     (ahash-set! toggle-table ',short ',long)
     (ahash-set! toggle-table ',long ',short)))

(define-summarize summarized detailed)
(define-summarize summarized-plain detailed-plain)
(define-summarize summarized-std detailed-std)
(define-summarize summarized-env detailed-env)
(define-summarize summarized-grouped detailed-grouped)

(ahash-set! toggle-table 'summarized-algorithm 'detailed-algorithm)
(ahash-set! toggle-table 'detailed-algorithm 'summarized-algorithm)

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
