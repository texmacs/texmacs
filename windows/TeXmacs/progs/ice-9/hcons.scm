;;; installed-scm-file

;;;; 	Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.
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


(define-module  (ice-9 hcons))


;;; {Eq? hash-consing}
;;;
;;; A hash conser maintains a private universe of pairs s.t. if 
;;; two cons calls pass eq? arguments, the pairs returned are eq?.
;;;
;;; A hash conser does not contribute life to the pairs it returns.
;;;

(define-public (hashq-cons-hash pair n)
  (modulo (logxor (hashq (car pair) 4194303)
		  (hashq (cdr pair) 4194303))
	  n))

(define-public (hashq-cons-assoc key l)
  (and (not (null? l))
       (or (and (pair? l)		; If not a pair, use its cdr?
		(pair? (car l))
		(pair? (caar l))
		(eq? (car key) (caaar l))
		(eq? (cdr key) (cdaar l))
		(car l))
	   (hashq-cons-assoc key (cdr l)))))

(define-public (hashq-cons-get-handle table key)
  (hashx-get-handle hashq-cons-hash hashq-cons-assoc table key #f))

(define-public (hashq-cons-create-handle! table key init)
  (hashx-create-handle! hashq-cons-hash hashq-cons-assoc table key init))

(define-public (hashq-cons-ref table key)
  (hashx-ref hashq-cons-hash hashq-cons-assoc table key #f))

(define-public (hashq-cons-set! table key val)
  (hashx-set! hashq-cons-hash hashq-cons-assoc table key val))

(define-public (hashq-cons table a d)
  (car (hashq-cons-create-handle! table (cons a d) #f)))

(define-public (hashq-conser hash-tab-or-size)
  (let ((table (if (vector? hash-tab-or-size)
		   hash-tab-or-size
		   (make-doubly-weak-hash-table hash-tab-or-size))))
    (lambda (a d) (hashq-cons table a d))))




(define-public (make-gc-buffer n)
  (let ((ring (make-list n #f)))
    (append! ring ring)
    (lambda (next)
      (set-car! ring next)
      (set! ring (cdr ring))
      next)))
