
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-drd.scm
;; DESCRIPTION : data relation definitions for educational content
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-drd)
  (:use (text text-drd)))

;; Enunciations from text-drd.scm

;;(define-group exercise-tag
;;  exercise problem question)
;;
;;(define-group solution-tag
;;  solution answer)


;; Buttons

(define-group button-tag
  button-box button-box* button-circle button-circle*
  button-arabic button-alpha button-Alpha button-roman button-Roman)

(define-group variant-tag (button-tag))
(define-group similar-tag (button-tag))

;; Buttons themes

(define-group with-button-tag
  with-button-box with-button-box* with-button-circle with-button-circle*
  with-button-arabic with-button-alpha with-button-Alpha
  with-button-roman with-button-Roman)

(define-group variant-tag (with-button-tag))
(define-group similar-tag (with-button-tag))

;; Multiple choice environments

(define-group mc-tag (mc-exclusive-tag) (mc-plural-tag))
(define-group variant-tag (mc-exclusive-tag) (mc-plural-tag))
(define-group similar-tag (mc-exclusive-tag) (mc-plural-tag))

(tm-define-macro (define-mc mc mcs)
  `(begin
     (define-group mc-exclusive-tag ,mc)
     (define-group mc-plural-tag ,mcs)
     (define-alternate ,mc ,mcs)))

(define-mc mc mcs)
(define-mc mc-monospaced mcs-monospaced)
(define-mc mc-horizontal mcs-horizontal)
(define-mc mc-vertical mcs-vertical)

;; Tags for text with gaps

(define-alternate hide-reply show-reply)

(define-group gap-tag (gap-short-tag) (gap-wide-tag) (gap-long-tag))
(define-group variant-tag (gap-short-tag) (gap-wide-tag) (gap-long-tag))
(define-group similar-tag (gap-short-tag) (gap-wide-tag) (gap-long-tag))

(tm-define-macro (define-gap gt gt-wide gt-long)
  `(begin
     (define-group gap-short-tag ,gt)
     (define-group gap-wide-tag ,gt-wide)
     (define-group gap-long-tag ,gt-long)
     (define-alternate ,gt ,gt-wide)))

(define-gap gap gap-wide gap-long)
(define-gap gap-dots gap-dots-wide gap-dots-long)
(define-gap gap-underlined gap-underlined-wide gap-underlined-long)
(define-gap gap-box gap-box-wide gap-box-long)
