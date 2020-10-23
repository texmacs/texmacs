
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
  (vspace-tag) (indent-tag) (new-page-tag) (page-break-tag)
  (move-tag) (resize-tag) (smash-tag) (inflate-tag) (reduce-by-tag)
  (font-effect-tag)
  (eff-tag) (basic-effect-tag) (color-effect-tag)
  (gaussian-effect-tag) (oval-effect-tag)
  (rectangular-effect-tag) (motion-effect-tag))

(define-group similar-tag
  (vspace-tag) (indent-tag) (new-page-tag) (page-break-tag)
  (move-tag) (resize-tag) (smash-tag) (inflate-tag) (reduce-by-tag)
  (font-effect-tag)
  (eff-tag) (basic-effect-tag) (color-effect-tag)
  (gaussian-effect-tag) (oval-effect-tag)
  (rectangular-effect-tag) (motion-effect-tag))

;; Various formatting tags

(define-group vspace-tag
  vspace vspace*)

(define-group indent-tag
  no-indent no-indent* yes-indent yes-indent*)

(define-group new-page-tag
  new-page new-page* new-dpage new-dpage*)

(define-group page-break-tag
  page-break page-break* no-break-here no-break-here*)

;; Various geometry adjustment tags

(define-group move-tag
  move shift)

(define-group resize-tag
  resize extend clipped)

(define-group smash-tag
  smash smash-bottom smash-top)

(define-group inflate-tag
  inflate inflate-bottom inflate-top)

(define-group reduce-by-tag
  reduce-by reduce-bottom-by reduce-top-by)

(define-group font-effect-tag
  embold embbb slanted hmagnified vmagnified
  condensed extended degraded distorted gnawed)

(define-group eff-tag
  eff-blur eff-outline eff-thicken eff-erode)

(define-group basic-effect-tag
  blur outline thicken erode
  shadow engrave emboss
  shadowed-raise outlined-engrave outlined-emboss
  degrade distort gnaw)

(define-group color-effect-tag
  make-transparent make-opaque recolor skin)

(define-group pen-effect-tag
  blur outline thicken erode
  (gaussian-effect-tag) (oval-effect-tag)
  (rectangular-effect-tag) (motion-effect-tag))
  
(define-group gaussian-effect-tag
  gaussian-blur gaussian-outline gaussian-thicken gaussian-erode)

(define-group oval-effect-tag
  oval-blur oval-outline oval-thicken oval-erode)

(define-group rectangular-effect-tag
  rectangular-blur rectangular-outline rectangular-thicken rectangular-erode)

(define-group motion-effect-tag
  motion-blur motion-outline motion-thicken motion-erode)
