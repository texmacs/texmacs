
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : scl-init.lisp
;; DESCRIPTION : Initialize scl plugin
;; COPYRIGHT   : (C) 2005 Douglas Crosher
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
