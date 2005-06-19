
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 78866f97-a87b-42c1-91d1-33142c08439f
;;
;; MODULE      : buffer-replace.scm
;; DESCRIPTION : Walking and changing the buffer
;; COPYRIGHT   : (C) 2004  David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (buffer-replace)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walking and changing the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (pred? p t) -> bool
;;   p: a path in the buffer tree
;;   t: the buffer subtree (path->tree) for that path
;;   returns: should transf be called, with saved position.
;;   Must not modify the buffer.
;;
;; (transf p t) -> unspecified
;;  p, t: same as for "pred?"
;;  Modify the buffer. Before calling this function, the current tree iteration
;;  position is saved as an editor position. The updated position, which may
;;  have been altered by editor actions in @transf, will be used for subsequent
;;  iterations.
;;
;; NOTE: robustness against buffer changes in @transf requires a tail recursive
;; implementation.

(define (protect-position p thunk cont)
  ;; Save @p as an editor position, execute @thunk, get the updated value of
  ;; @p, deleted the position, and apply the updated value to @cont.
  (let ((pos (position-new)))
    (position-set pos p)
    (thunk)
    (let ((p (position-get pos)))
      (position-delete pos)
      (cont p))))

(tm-define buffer-replace 
  (case-lambda
    ((order pred? transf)
     (cond ((eqv? :pre order)
	    (buffer-replace-preorder pred? transf))
	   ((eqv? :post order)
	    (buffer-replace-postorder pred? transf))
	   (else
	    (error "Bad order keyword:" order))))
    ((order p pred? transf)
     (cond ((eqv? :pre order)
	    (buffer-replace-preorder-from p pred? transf))
	   ((eqv? :post order)
	    (buffer-replace-postorder-from p pred? transf))
	   (else
	    (error "Bad order keyword:" order))))))
  
(tm-define (buffer-replace-preorder pred? transf)
  ;; Preorder traversal, do not recurse in matching subtrees.
  (buffer-replace-preorder-from '() pred? transf))

(tm-define (buffer-replace-preorder-from p pred? transf)
  (let ((t (path->tree p)))
    (cond ((pred? p t)
	   (protect-position
	    (rcons p 0) (cut transf p t)
	    (lambda (pos)
	      (buffer-replace-preorder/right (cDr pos) pred? transf))))
	  ((and (tree-compound? t)
		(< 0 (tree-arity t)))
	   (buffer-replace-preorder-from (rcons p 0) pred? transf))
	  (else (buffer-replace-preorder/right p pred? transf)))))

(define (buffer-replace-preorder/right p pred? transf)
  (let ascend ((ip (reverse p)))
    (if (pair? ip)
	(let* ((i (1+ (car ip)))
	       (ipp (cdr ip))
	       (t (path->tree (reverse ipp))))
	  ;; by construction, t is a compound tree
	  (if (< i (tree-arity t))
	      (buffer-replace-preorder-from
	       (reverse (cons i ipp)) pred? transf)
	      (ascend ipp))))))

(tm-define (buffer-replace-postorder pred? transf)
  ;; Postorder traversal, all subtrees are walked.
  (buffer-replace-postorder-from '() pred? transf))

(tm-define (buffer-replace-postorder-from p pred? transf)
  (let ((t (path->tree p)))
    (if (and (tree-compound? t)
	     (< 0 (tree-arity t)))
	(buffer-replace-postorder-from (rcons p 0) pred? transf)
	(buffer-replace-postorder/right p pred? transf))))

(define (buffer-replace-postorder/right p pred? transf)
  (let ascend ((ip (reverse p)))
    (if (pair? ip)
	(let* ((i (1+ (car ip)))
	       (ipp (cdr ip))
	       (t (path->tree (reverse ipp))))
	  ;; by construction, t is a compound tree
	  (cond ((< i (tree-arity t))
		 (buffer-replace-postorder-from
		  (reverse (cons i ipp)) pred? transf))
		((pred? (reverse ipp) t)
		 (protect-position
		  (reverse (cons 0 ipp)) (cut transf (reverse ipp) t)
		  (lambda (pos) (ascend (cdr (reverse pos))))))
		(else (ascend ipp)))))))
