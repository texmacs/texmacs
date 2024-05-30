;;; examples/modules/module-1.scm -- Module system demo.

;;; Commentary:

;;; Module 1 of the module demo program.

;;; Author: Martin Grabmueller
;;; Date: 2001-05-29

;;; Code:

(define-module (module-1))

(export foo bar)

(define (foo)
  (display "module-1 foo")
  (newline))

(define (bar)
  (display "module-1 bar")
  (newline))

;;; End of file.
