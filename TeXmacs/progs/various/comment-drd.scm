
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

(define-group hidden-comment-tag
  folded-comment)

(define-group comment-tag
  folded-comment unfolded-comment)

(define-group visible-comment-tag
  (comment-tag) nested-comment)

;; Hidden comments

(define-group invisible-hidden-comment-tag
  invisible-folded-comment)

(define-group invisible-comment-tag
  invisible-folded-comment invisible-unfolded-comment invisible-nested-comment)

(define-group any-comment-tag
  (visible-comment-tag) (invisible-comment-tag))

(define-group any-hidden-comment-tag
  (hidden-comment-tag) (invisible-hidden-comment-tag))
