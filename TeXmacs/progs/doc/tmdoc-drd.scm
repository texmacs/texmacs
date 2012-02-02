
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-drd.scm
;; DESCRIPTION : data relation definitions for TeXmacs documentation
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-drd)
  (:use (utils edit variants)))

;; General groups

(define-group variant-tag
  (tmdoc-traversal-tag) (tmdoc-menu-tag)
  (tmdoc-annotation-tag) (tmdoc-style-annotation-tag)
  (tmdoc-small-box-tag) (tmdoc-big-box-tag))

(define-group similar-tag
  (tmdoc-traversal-tag) (tmdoc-menu-tag)
  (tmdoc-annotation-tag) (tmdoc-style-annotation-tag)
  (tmdoc-small-box-tag) (tmdoc-big-box-tag))

;; TeXmacs documentation groups

(define-group tmdoc-traversal-tag
  branch extra-branch continue)

(define-group tmdoc-menu-tag
  menu submenu subsubmenu subsubsubmenu)

(define-group tmdoc-annotation-tag
  markup src-arg src-var src-length src-tt src-numeric src-textual)

(define-group tmdoc-style-annotation-tag
  tmstyle tmpackage tmdtd)

(define-group tmdoc-small-box-tag
  small-focus small-envbox)

(define-group tmdoc-big-box-tag
  big-focus big-envbox)
