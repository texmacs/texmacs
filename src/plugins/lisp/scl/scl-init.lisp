
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : scl-init.lisp
;; DESCRIPTION : Initialize scl plugin
;; COPYRIGHT   : (C) 2005 Douglas Crosher
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
