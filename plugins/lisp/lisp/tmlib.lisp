;; Return a tree with the 'tree symbol inserted for TeXmacs coding
(defun make-tm-tree (X)
	(if (equal X nil)
		nil
		(cons 'tree (tmtrec X))))

;; Co-function with make-tm-tree 
(defun tmtrec (X)
	(if (equal X NIL) NIL 
	  (if (listp (first X))
		(append (list (make-tm-tree (first X))) (tmtrec (rest X)))
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
(defun tree-ref (L A)
	(if (equal A '())
	    L
	    (tree-ref (nth (car A) L) (cdr A))))

;; Set a node of a tree
(defun tree-set (L A V)
	(if (equal A '())
	    V
	    (concatenate 'list 
			(subseq L 0 (car A)) 
			(list (tree-set (car (subseq L (car A) (1+ (car A)))) (cdr A) V))
			(subseq L (1+ (car A))))))


