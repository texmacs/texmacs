;;;; 	Copyright (C) 1996, 1998 Free Software Foundation, Inc.
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
;;;; ----------------------------------------------------------------
;;;; threads.scm -- User-level interface to Guile's thread system
;;;; 4 March 1996, Anthony Green <green@cygnus.com>
;;;; Modified 5 October 1996, MDJ <djurfeldt@nada.kth.se>
;;;; ----------------------------------------------------------------
;;;;


(define-module (ice-9 threads))



; --- MACROS -------------------------------------------------------

(define-public (%thread-handler tag . args)
  (fluid-set! the-last-stack #f)
  (unmask-signals)
  (let ((n (length args))
	(p (current-error-port)))
  (display "In thread:" p)
  (newline p)
  (if (>= n 3)
      (display-error #f
		     p
		     (car args)
		     (cadr args)
		     (caddr args)
		     (if (= n 4)
			 (cadddr args)
			 '()))
      (begin
	(display "uncaught throw to " p)
	(display tag p)
	(display ": " p)
	(display args p)
	(newline p)))))

(defmacro-public make-thread (fn . args)
  `(call-with-new-thread
    (lambda ()
      (,fn ,@args))
    %thread-handler))

(defmacro-public begin-thread (first . thunk)
  `(call-with-new-thread
    (lambda ()
      (begin
	,first ,@thunk))
    %thread-handler))

(defmacro-public with-mutex (m . thunk)
  `(dynamic-wind
    (lambda () (lock-mutex ,m))
    (lambda () (begin ,@thunk))
    (lambda () (unlock-mutex ,m))))

(defmacro-public monitor (first . thunk)
  `(with-mutex ,(make-mutex)
    (begin
      ,first ,@thunk)))
