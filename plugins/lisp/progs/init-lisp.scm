
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-lisp.scm
;; DESCRIPTION : Initialize lisp plugins
;; COPYRIGHT   : (C) 2004 Free Software Foundation
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lisp-initialize)
	(import-from (utils plugins plugin-convert))
	(plugin-input-converters lisp))

(define (lisp-versions)
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
  (:initialize (lisp-initialize))
  ,@(lisp-versions)
  (:session "Lisp"))
