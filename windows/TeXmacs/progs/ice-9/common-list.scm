;;;; common-list.scm --- COMMON LISP list functions for Scheme
;;;;
;;;; 	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

(define-module (ice-9 common-list))

;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define-public (adjoin e l) 
  "Returns list L, possibly with element E added if it is not already in L."
  (if (memq e l) l (cons e l)))

(define-public (union l1 l2)
  "Returns a new list that is the union of L1 and L2.
Elements that occur in both lists will occur only once
in the result list."
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (union (cdr l1) (adjoin (car l1) l2)))))

(define-public (intersection l1 l2)
  "Returns a new list that is the intersection of L1 and L2.
Only elements that occur in both lists will occur in the result list."
  (cond ((null? l1) l1)
	((null? l2) l2)
	((memv (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
	(else (intersection (cdr l1) l2))))

(define-public (set-difference l1 l2)
  "Return elements from list L1 that are not in list L2."
  (cond ((null? l1) l1)
	((memv (car l1) l2) (set-difference (cdr l1) l2))
	(else (cons (car l1) (set-difference (cdr l1) l2)))))

(define-public (reduce-init p init l)
  "Same as `reduce' except it implicitly inserts INIT at the start of L."
  (if (null? l)
      init
      (reduce-init p (p init (car l)) (cdr l))))

(define-public (reduce p l)
  "Combines all the elements of sequence L using a binary operation P.
The combination is left-associative. For example, using +, one can
add up all the elements. `reduce' allows you to apply a function which
accepts only two arguments to more than 2 objects.  Functional
programmers usually refer to this as foldl."
  (cond ((null? l) l)
	((null? (cdr l)) (car l))
	(else (reduce-init p (car l) (cdr l)))))

(define-public (some pred l . rest)
  "PRED is a boolean function of as many arguments as there are list
arguments to `some'. I.e., L plus any optional arguments. PRED is
applied to successive elements of the list arguments in order. As soon
as one of these applications returns a true value, `some' terminates
and returns that value.  If no application returns a true value,
`some' returns #f. All the lists should have the same length."
  (cond ((null? rest)
	 (let mapf ((l l))
	   (and (not (null? l))
		(or (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(and (not (null? l))
		     (or (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (every pred l . rest)
  "Return #t iff every application of PRED to L, etc., returns #t.
Analogous to `some' except it returns #t if every application of
PRED is #t and #f otherwise."
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

(define-public (notany pred . ls) 
  "Return #t iff every application of PRED to L, etc., returns #f.
Analogous to some but returns #t if no application of PRED returns a
true value or #f as soon as any one does."
  (not (apply some pred ls)))

(define-public (notevery pred . ls) 
  "Return #t iff there is an application of PRED to L, etc., that returns #f.
Analogous to some but returns #t as soon as an application of PRED returns #f,
or #f otherwise."
  (not (apply every pred ls)))

(define-public (find-if pred l)
  "Searches for the first element in L such that (PRED element)
returns true. If it finds any such element in L, element is
returned. Otherwise, #f is returned."
  (cond ((null? l) #f)
	((pred (car l)) (car l))
	(else (find-if pred (cdr l)))))

(define-public (member-if pred l)
  "Returns L if (T element) is true for any element in L.  Returns #f
if PRED does not apply to any element in L."
  (cond ((null? l) #f)
	((pred (car l)) l)
	(else (member-if pred (cdr l)))))

(define-public (remove-if p l)
  "Removes all elements from L where (P element) is true.
Returns everything that's left."
  (cond ((null? l) '())
	((p (car l)) (remove-if p (cdr l)))
	(else (cons (car l) (remove-if p (cdr l))))))

(define-public (remove-if-not p l)
  "Removes all elements from L where (P element) is #f.
Returns everything that's left."
  (cond ((null? l) '())
	((not (p (car l))) (remove-if-not p (cdr l)))
	(else (cons (car l) (remove-if-not p (cdr l))))))

(define-public (delete-if! pred list)
  "Destructive version of `remove-if'."
  (let delete-if ((list list))
    (cond ((null? list) '())
	  ((pred (car list)) (delete-if (cdr list)))
	  (else
	   (set-cdr! list (delete-if (cdr list)))
	   list)))) 

(define-public (delete-if-not! pred list)
  "Destructive version of `remove-if-not'."
  (let delete-if-not ((list list))
    (cond ((null? list) '())
	  ((not (pred (car list))) (delete-if-not (cdr list)))
	  (else
	   (set-cdr! list (delete-if-not (cdr list)))
	   list))))

(define-public (butlast lst n)
  "Return all but the last N elements of LST."
  (letrec ((l (- (length lst) n))
	   (bl (lambda (lst n)
		 (cond ((null? lst) lst)
		       ((positive? n)
			(cons (car lst) (bl (cdr lst) (+ -1 n))))
		       (else '())))))
    (bl lst (if (negative? n)
		(error "negative argument to butlast" n)
		l))))

(define-public (and? . args)
  "Return #t iff all of ARGS are true."
  (cond ((null? args) #t)
	((car args) (apply and? (cdr args)))
	(else #f)))

(define-public (or? . args)
  "Return #t iff any of ARGS is true."
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply or? (cdr args)))))

(define-public (has-duplicates? lst)
  "Return #t iff 2 members of LST are equal?, else #f."
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (has-duplicates? (cdr lst)))))

(define-public (pick p l)
  "Apply P to each element of L, returning a list of elts
for which P returns a non-#f value."
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l))	(loop (cons (car l) s) (cdr l)))
     (else		(loop s (cdr l))))))

(define-public (pick-mappings p l)
  "Apply P to each element of L, returning a list of the 
non-#f return values of P."
  (let loop ((s '())
	     (l l))
    (cond
     ((null? l) 	s)
     ((p (car l)) =>	(lambda (mapping) (loop (cons mapping s) (cdr l))))
     (else		(loop s (cdr l))))))

(define-public (uniq l)
  "Return a list containing elements of L, with duplicates removed."
  (if (null? l)
      '()
      (let ((u (uniq (cdr l))))
	(if (memq (car l) u)
	    u
	    (cons (car l) u)))))

