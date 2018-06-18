
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : poster-drd.scm
;; DESCRIPTION : data relation definitions for poster styles
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various poster-drd)
  (:use (utils edit variants)))

(define-group variant-tag
  (titled-block-tag) (untitled-block-tag))

(define-group titled-block-tag
  plain-titled-block framed-titled-block alternate-titled-block)

(define-group untitled-block-tag
  plain-block framed-block alternate-block)

(define-group poster-block-tag
  (titled-block-tag) (untitled-block-tag))
