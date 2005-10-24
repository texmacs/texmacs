
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : tmlib.lisp
;; DESCRIPTION : Lisp additions
;; COPYRIGHT   : (C) 2004 Michael Graffam
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a tree with the 'tree symbol inserted for TeXmacs coding
(defun make-tm-tree (X)
	(if (equal X nil)
		nil
		(cons 'tree (tmtrec X))))

;; Co-function with make-tm-tree 
(defun tmtrec (x)
	(if (equal x nil) nil 
	  (if (listp (first x))
		(append (list (make-tm-tree (first x))) (tmtrec (rest x)))
		(append (list (first X)) (tmtrec (rest X))))))

;; Return a string with all the coding needed to display it in TeXmacs
(defun make-tm-string (s x)
	(cond 
	   ( (and (equal s 'tree) (listp x)) 
		(concatenate 'string tm-data-begin "scheme:"
			(string-downcase (princ-to-string (make-tm-tree x))) tm-data-end) )
	   ( (and (equal s 'scheme) (listp x))
		(concatenate 'string tm-data-begin "scheme:"
			(string-downcase (prin1-to-string x)) tm-data-end) )
	   ( (and (equal s 'command) (listp x))
		(concatenate 'string tm-data-begin "command:"
			(string-downcase (princ-to-string x)) tm-data-end))))

;; Reference a node of a tree
(defun tree-ref (l a)
	(if (equal a '())
	    l
	    (tree-ref (nth (car a) l) (cdr a))))

;; Set a node of a tree
(defun tree-set (l a v)
	(if (equal a '())
	    v
	    (concatenate 'list 
			(subseq l 0 (car a)) 
			(list (tree-set (car (subseq l (car a) (1+ (car a)))) (cdr a) v))
			(subseq l (1+ (car a))))))


