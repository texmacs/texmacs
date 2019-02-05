
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-edit.scm
;; DESCRIPTION : editing routines for educational purposes
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-edit)
  (:use (dynamic fold-edit)
        (education edu-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (problem-context? t)
  (exercise-tag? (tree-label t)))

(tm-define (solution-context? t)
  (solution-tag? (tree-label t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating on a tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edu-operate-document l mode)
  (cond ((null? l) (noop))
        ((and (nnull? (cdr l))
              (problem-context? (car l))
              (solution-context? (cadr l)))
         (edu-operate-document (cddr l) mode)
         (let* ((prb (car l))
                (sol (cadr l))
                (tag (if (== mode :problem) 'folded 'unfolded)))
           (tree-insert-node! prb 0 (list tag))
           (tree-insert-node! sol 0 (list tag))
           (tree-join (tree-up prb) (tree-index prb))))
        (else
         (edu-operate-document (cdr l) mode)
         (edu-operate (car l) mode))))

(tm-define (edu-operate t mode)
  (cond ((tree-atomic? t) (noop))
        ((toggle-first-context? t)
         (cond ((== mode :solution)
                (dynamic-operate t :unfold))))
        ((toggle-second-context? t)
         (cond ((== mode :problem)
                (dynamic-operate t :fold))))
        ((tree-is? t 'document)
         (edu-operate-document (tree-children t) mode))
        (else
          (for-each (cut edu-operate <> mode) (tree-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting the main operation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edu-set-mode mode)
  (edu-operate (buffer-tree) mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggling buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mouse-toggle-button t)
  (:type (-> void))
  (:synopsis "Toggle a button using the mouse")
  (:secure #t)
  (cond ((tm-equal? t "true" ) (tree-set! t "false"))	
	((tm-equal? t "false") (tree-set! t "true" ))))

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
