
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for documentation
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-drd))

;; General groups

(define-group variant-tag
  (fragment-tag))

(define-group similar-tag
  (fragment-tag))

;; Special markup categories

(define-group fragment-tag
  framed-fragment shell-fragment scm-fragment cpp-fragment mmx-fragment)

(tm-define (variant-circulate t forward?)
  (:mode in-tmdoc?)
  (:require (tree-in? t (tmdoc-prog-tag-list)))
  (variant-circulate-in t (tmdoc-prog-tag-list) forward?))
