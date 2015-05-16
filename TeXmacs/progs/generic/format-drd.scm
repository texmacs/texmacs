
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-drd.scm
;; DESCRIPTION : data relation definitions for formatting tags
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-drd)
  (:use (utils edit variants)))

;; General groups

(define-group variant-tag
  (move-tag) (resize-tag) (smash-tag) (swell-tag) (reduce-by-tag))

(define-group similar-tag
  (move-tag) (resize-tag) (smash-tag) (swell-tag) (reduce-by-tag))

;; Various geometry adjustment tags

(define-group move-tag
  move shift)

(define-group resize-tag
  resize extend clipped)

(define-group smash-tag
  smash smash-bottom smash-top)

(define-group swell-tag
  swell swell-bottom swell-top)

(define-group reduce-by-tag
  reduce-by reduce-bottom-by reduce-top-by)
