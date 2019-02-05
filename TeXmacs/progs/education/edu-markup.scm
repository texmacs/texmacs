
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-markup.scm
;; DESCRIPTION : special external markup for educational purposes
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-markup)
  (:use (database title-markup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (title-prefix pre tit)
  (cond ((tm-is? tit 'doc-make-rich-title)
	 (with l (tm-children tit)
	   `(doc-make-rich-title ,@(cDr l) ,(title-prefix pre (cAr l)))))
	((tm-is? tit 'document)
	 `(document ,pre ,@(tm-children tit)))
	(else tit)))

(tm-define (doc-data-exam t xopts)
  (:secure #t)
  (let* ((doc   (doc-data t xopts))
	 (class (select t '(doc-exam-class :%1)))
	 (date  (select t '(doc-exam-date :%1))))
    (cond ((and (null? class) (null? date))
	   doc)
	  ((and (nnull? class) (null? date))
	   (title-prefix `(doc-exam-class ,(car class)) doc))
	  ((and (null? class) (nnull? date))
	   (title-prefix `(doc-exam-date ,(car date)) doc))
	  ((and (nnull? class) (nnull? date))
	   (title-prefix
	    `(doc-exam-class-date ,(car class) ,(car date)) doc)))))
