(setf tm-data-begin "")
(setf tm-data-end "")

(setf *prompt* 
	(concatenate 'string tm-data-begin "channel:prompt" tm-data-end
 	   "CMUCL> " tm-data-end))

(defun tm-debug-prompt ()
	(princ (concatenate 'string tm-data-begin "channel:prompt" 
		  tm-data-end "CMUCL-Debug] " tm-data-end))
	(force-output))

(setf debug:*debug-prompt* #'tm-debug-prompt)

(load "lisp/tmlib.lisp")
