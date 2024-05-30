;;; examples/modules/module-2.scm -- Module system demo.

;;; Commentary:

;;; Module 2 of the module demo program.

;;; Author: Martin Grabmueller
;;; Date: 2001-05-29

;;; Code:

(define-module (module-2))

(export foo bar braz)

(define (foo)
  (display "module-2 foo")
  (newline))

(define (bar)
  (display "module-2 bar")
  (newline))

(define (braz)
  (display "module-2 braz")
  (newline))

;;; End of file.
