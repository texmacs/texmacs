
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


;; Toggle buttons

(define-group button-tag (button-off-tag) (button-on-tag))
(define-group variant-tag (button-off-tag) (button-on-tag))
(define-group similar-tag (button-off-tag) (button-on-tag))

(tm-define-macro (define-button folded unfolded)
  `(begin
     (define-group button-off-tag ,folded)
     (define-group button-on-tag ,unfolded)
     (define-alternate ,folded ,unfolded)))

(define-button box-off box-on)
(define-button cross-off cross-on)
(define-button circle-off circle-on)
(define-button numeric-off numeric-on)
(define-button alpha-off alpha-on)

;; Multiple choice environments

(define-group mc-tag (multiple-choice-tag) (multiple-choices-tag))
(define-group variant-tag (multiple-choice-tag) (multiple-choices-tag))
(define-group similar-tag (multiple-choice-tag) (multiple-choices-tag))

(tm-define-macro (define-mc mc mcs)
  `(begin
     (define-group multiple-choice-tag ,mc)
     (define-group multiple-choices-tag ,mcs)
     (define-alternate ,mc ,mcs)))

(define-mc mc-basic mcs-basic)
(define-mc mc-std mcs-std)
(define-mc mc-list mcs-list)
