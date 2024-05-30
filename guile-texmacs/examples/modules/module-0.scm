;;; examples/modules/module-0.scm -- Module system demo.

;;; Commentary:

;;; Module 0 of the module demo program.

;;; Author: Martin Grabmueller
;;; Date: 2001-05-29

;;; Code:

(define-module (module-0))

(export foo bar)

(define (foo)
  (display "module-0 foo")
  (newline))

(define (bar)
  (display "module-0 bar")
  (newline))

;;; End of file.
