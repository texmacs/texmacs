
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-lisp.scm
;; DESCRIPTION : Initialize lisp plugins
;; COPYRIGHT   : (C) 2004 Free Software Foundation
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lisp-launchers)
  (let ((version-list
	 (append (if (url-exists-in-path? "clisp") '("Clisp") '())
		 (if (url-exists-in-path? "lisp") '("Cmucl") '())
		 (if (url-exists-in-path? "scl") '("Scl") '()))))
    (if (nnull? version-list)
      (let* ((default (car version-list))
	     (rest (cdr version-list))
	     (launch-default
	      (list :launch (string-append "tm_lisp " default)))
	     (launch-rest
	      (map
	       (lambda (version-name)
		 (list :launch version-name
		       (string-append "tm_lisp " version-name)))
	       rest)))
	(cons launch-default launch-rest))
      '())))

(plugin-configure lisp
  (:require (or (url-exists-in-path? "clisp")
		(url-exists-in-path? "lisp")
		(url-exists-in-path? "scl")))
  ,@(lisp-launchers)
  (:session "Lisp"))

(tm-cond-expand (supports-lisp?)
  (plugin-input-converters lisp))
