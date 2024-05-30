;;; examples/safe/evil.scm -- Evil Scheme file to be run in a safe
;;; environment.

;;; Commentary:

;;; This is an example file to be evaluated by the `safe' program in
;;; this directory.  This program, unlike the `untrusted.scm' (which
;;; is untrusted, but a really nice fellow though), tries to do evil
;;; things and will thus break in a safe environment.
;;;
;;; *Note* that the files in this directory are only suitable for
;;; demonstration purposes, if you have to implement safe evaluation
;;; mechanisms in important environments, you will have to do more
;;; than shown here -- for example disabling input/output operations.

;;; Author: Martin Grabmueller
;;; Date: 2001-05-30

;;; Code:

(define passwd (open-input-file "/etc/passwd"))

(let lp ((ch (read-char passwd)))
  (if (not (eof-object? ch))
    (lp (read-char passwd))))

;;; End of file.
