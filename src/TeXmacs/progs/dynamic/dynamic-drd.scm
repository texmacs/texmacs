
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

;; Variants

; (define-group variant-tag
;   (fold-tag) (unfold-tag))

; (define-group similar-tag
;   (fold-tag) (unfold-tag))

;; Fold-unfold

; (tm-define fold-table (make-ahash-table))
; (tm-define-macro (define-toggle folded unfolded)
;   `(begin
;      (define-group fold-tag ,folded)
;      (define-group unfold-tag ,unfolded)
;      (ahash-set! fold-table ,folded ,unfolded)
;      (ahash-set! fold-table ,unfolded ,folded)))

; (define-toggle fold unfold)

;; Switches

(define-group switch-tag
  (alternative-tag) (unroll-tag) (expanded-tag))

(define-group big-switch-tag
  new-switch unroll expanded)

(define-group alternative-tag
  new-switch)

(define-group unroll-tag
  unroll)

(define-group expanded-tag
  expanded)
