;;;; 	Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation
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
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;;


(define-module (ice-9 debug))


;;; {Misc}
;;;
(define-public (frame-number->index n . stack)
  (let ((stack (if (null? stack)
		   (fluid-ref the-last-stack)
		   (car stack))))
    (if (memq 'backwards (debug-options))
	n
	(- (stack-length stack) n 1))))


;;; {Trace}
;;;
;;; This code is just an experimental prototype (e. g., it is not
;;; thread safe), but since it's at the same time useful, it's
;;; included anyway.
;;;
(define traced-procedures '())

(define-public (trace . args)
  (if (null? args)
      (nameify traced-procedures)
      (begin
	(for-each (lambda (proc)
		    (if (not (procedure? proc))
			(error "trace: Wrong type argument:" proc))
		    (set-procedure-property! proc 'trace #t)
		    (if (not (memq proc traced-procedures))
			(set! traced-procedures
			      (cons proc traced-procedures))))
		  args)
	(set! apply-frame-handler trace-entry)
	(set! exit-frame-handler trace-exit)
	(set! trace-level 0)
	(debug-enable 'trace)
	(nameify args))))

(define-public (untrace . args)
  (if (and (null? args)
	   (not (null? traced-procedures)))
      (apply untrace traced-procedures)
      (begin
	(for-each (lambda (proc)
		    (set-procedure-property! proc 'trace #f)
		    (set! traced-procedures (delq! proc traced-procedures)))
		  args)
	(if (null? traced-procedures)
	    (debug-disable 'trace))
	(nameify args))))

(define (nameify ls)
  (map (lambda (proc)
	 (let ((name (procedure-name proc)))
	   (or name proc)))
       ls))

(define trace-level 0)
(add-hook! abort-hook (lambda () (set! trace-level 0)))

(define (trace-entry key cont tail)
  (if (eq? (stack-id cont) 'repl-stack)
      (let ((cep (current-error-port))
	    (frame (last-stack-frame cont)))
	(if (not tail)
	    (set! trace-level (+ trace-level 1)))
	(let indent ((n trace-level))
	  (cond ((> n 1) (display "|  " cep) (indent (- n 1)))))
	(display-application frame cep)
	(newline cep)))
  ;; It's not necessary to call the continuation since
  ;; execution will continue if the handler returns
  ;(cont #f)
  )

(define (trace-exit key cont retval)
  (if (eq? (stack-id cont) 'repl-stack)
      (let ((cep (current-error-port)))
	(set! trace-level (- trace-level 1))
	(let indent ((n trace-level))
	  (cond ((> n 0) (display "|  " cep) (indent (- n 1)))))
	(write retval cep)
	(newline cep))))


;;; A fix to get the error handling working together with the module system.
;;;
(variable-set! (builtin-variable 'debug-options) debug-options)



(debug-enable 'debug)
(read-enable 'positions)
