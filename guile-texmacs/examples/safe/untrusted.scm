;;; examples/safe/untrusted.scm -- Scheme file to be run in a safe
;;; environment.

;;; Commentary:

;;; This is an example file to be evaluated by the `safe' program in
;;; this directory.
;;;
;;; *Note* that the files in this directory are only suitable for
;;; demonstration purposes, if you have to implement safe evaluation
;;; mechanisms in important environments, you will have to do more
;;; than shown here -- for example disabling input/output operations.

;;; Author: Martin Grabmueller
;;; Date: 2001-05-30

;;; Code:

;; fact -- the everlasting factorial function...
;;
(define (fact n)
  (if (< n 2)
    1
    (* n (fact (- n 1)))))

;; Display the factorial of 0..9 to the terminal.
;;
(do ((x 0 (+ x 1)))
    ((= x 11))
  (display (fact x))
  (newline))

;;; End of file.
