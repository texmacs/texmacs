
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 9fd641ba-73fe-41e6-a4b7-d0043f5efa2e
;;
;; MODULE      : proclus-list.scm
;; DESCRIPTION : List library for Proclus
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
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

(texmacs-module (proclus-list))

(tm-define (quadripartite l)
  ;; (a b c d e fg h)->((a b c d) (e f g h ))
  ;; suppose que l a 3*n terms
  (if (pair? l)
      (cons  (list (first l) (second l) (third l) (fourth l))
	     (quadripartite (cddddr l)))
      '()))

(tm-define (transform pred? transf l)
  ;; Traverse @l recursively in preorder, when an object matches @pred?,
  ;; replace it by the value of (@transf l) and do not descend.
  ;;
  ;; The root @l is not tested, and if @l is not a pair, it is returned without
  ;; change.
  (let loop ((l l))
    (if (pair? l)
	(append (list (loop (if (pred? (car l))
				(transf (car l))
				(car l))))
		(loop (cdr l)))
	l)))


(tm-define (extract pred? lst)
  ;; Traverse @lst recursively in preorder and return the list of all objects
  ;; matching @pred?.
  ;;
  ;; Example : (extract pair? lst) returns the list of all nested lists in lst.
  (let sub ((lst lst))
    (if (pair? lst)
        (append (let ((head (car lst)))
                  (if (pred? head) (list head) '()))
                (sub (car lst))
                (sub (cdr lst)))
        '())))

(tm-define (disjoint? e1 e2)
  (null? (intersection e1 e2)))

(define (intersection  e1 e2)
  (cond ((null? e1) e1)
	((in? (car e1) e2) (cons (car e1)
				     (intersection (cdr e1) e2)))
	(else (intersection (cdr e1) e2))))


(tm-define (remove term list)
  (list-filter list (lambda (x) (not (equal? x term)))))

(tm-define (last-four l)
  (list-n-last l 4))

(define (list-n-last l n)
  ;; Last @n items of @l, or @l if length(@l)<@n.
  (if (< (length l) n) l (list-take-right l n)))

(tm-define (but-last-four l)
  (remove-n-last l 4))

(tm-define (remove-n-last l n)
  (if (< n (length l))
      (list-drop-right list n)
      '()))

(define (remove-n-first l n)
  ;; Remove the first @n items of @l, or empty list if length(@l)<@n.
  (if (< n (length l))
      (list-drop list n)
      '()))

(tm-define (no-repetition-list list)
(if (null? list) '() (cons (car list) (remove (car list) (no-repetition-list (cdr list)))))
) 
