;;;; and-let*.scm --- and-let* syntactic form (draft SRFI-2) for Guile
;;;; written by Michael Livshin <mike@olan.com>
;;;;
;;;; 	Copyright (C) 1999 Free Software Foundation, Inc.
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

(define-module (ice-9 and-let*))

(defmacro and-let* (vars . body)

  (define (expand vars body)
    (cond
     ((null? vars)
      `(begin ,@body))
     ((pair? vars)
      (let ((exp (car vars)))
        (cond
         ((pair? exp)
          (cond
           ((null? (cdr exp))
            `(and ,(car exp) ,(expand (cdr vars) body)))
           (else
            (let ((var (car exp))
                  (val (cadr exp)))
              `(let (,exp)
                 (and ,var ,(expand (cdr vars) body)))))))
         (else
          `(and ,exp ,(expand (cdr vars) body))))))
     (else
      (error "not a proper list" vars))))

  (expand vars body))

(export-syntax and-let*)
