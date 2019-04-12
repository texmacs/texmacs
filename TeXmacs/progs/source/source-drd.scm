
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-drd.scm
;; DESCRIPTION : data relation definitions for source mode
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source source-drd)
  (:use (utils edit variants)))

;; General groups

(define-group variant-tag
  (assign-tag))

(define-group similar-tag
  (assign-tag))

;; Variable tags

(define-group assign-tag
  assign provide)
