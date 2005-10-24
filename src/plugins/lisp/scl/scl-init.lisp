(setf tm-data-begin "")
(setf tm-data-end "")

(setf *prompt* 
	(concatenate 'string tm-data-begin "channel:prompt" tm-data-end
 	   "SCL> " tm-data-end))

(defun tm-debug-prompt ()
	(princ (concatenate 'string tm-data-begin "channel:prompt" 
		  tm-data-end "SCL-Debug] " tm-data-end))
	(force-output))

(setf debug:*debug-prompt* #'tm-debug-prompt)

(load "lisp/tmlib.lisp")
