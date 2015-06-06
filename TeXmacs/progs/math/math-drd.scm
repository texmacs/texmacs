
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-drd.scm
;; DESCRIPTION : data relation definitions for math mode
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-drd)
  (:use (utils edit variants)))

(define-group variant-tag
  (fraction-tag) (vertical-script-tag)
  (textual-operator-tag))

(define-group fraction-tag
  frac tfrac dfrac frac* cfrac)

(define-group vertical-script-tag
  below above)

(define-group textual-operator-tag
  math-up math-ss math-tt math-bf math-it math-sl)

(define-group math-annotation-tag
  math-separator math-quantifier math-imply math-or math-and
  math-not math-relation math-union math-intersection math-exclude
  math-plus math-minus math-times math-over math-big
  math-prefix math-postfix math-open math-close math-ordinary math-ignore)

(define-alternate wide wide*)
(define-alternate around around*)
