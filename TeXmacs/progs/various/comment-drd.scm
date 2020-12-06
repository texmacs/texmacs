
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-drd.scm
;; DESCRIPTION : various types of comments
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-drd)
  (:use (utils edit variants)))

;; General groups

(define-group variant-tag
  (comment-tag))

(define-group similar-tag
  (comment-tag))

;; Comments

(define-group folded-comment-tag
  folded-comment)

(define-group comment-tag
  folded-comment unfolded-comment)

(define-group shown-comment-tag
  (comment-tag) nested-comment)

;; Hidden comments

(define-group hidden-folded-comment-tag
  hidden-folded-comment)

(define-group hidden-comment-tag
  hidden-folded-comment hidden-unfolded-comment hidden-nested-comment)

(define-group any-comment-tag
  (shown-comment-tag) (hidden-comment-tag)
  mirror-comment carbon-comment)

(define-group any-folded-comment-tag
  (folded-comment-tag) (hidden-folded-comment-tag))
