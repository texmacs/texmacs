(in-package "MAXIMA")

(DEFUN MAIN-PROMPT ()
  (FORMAT () "(~A~D) "
    (STRIPDOLLAR $INCHAR) $LINENUM))

;(DEFUN BREAK-PROMPT ()
;  (declare (special $prompt))
;  (format nil "~A" (STRIPDOLLAR $PROMPT)))

(DEFMFUN DISPLA (FORM &aux #+kcl(form form))
  (IF (OR (NOT #.TTYOFF) #.WRITEFILEP)
      (cond #+Franz ($typeset (apply #'$photot (list form)))
            ((eq $display2d '$texmacs) (latex form))
	    ($DISPLAY2D
	     (LET ((DISPLAYP T)
		   (LINEARRAY (IF DISPLAYP (MAKE-array 80.) LINEARRAY))
		   (MRATP (CHECKRAT FORM))
		   (#.WRITEFILEP #.WRITEFILEP)
		   (MAXHT     1) (MAXDP   0) (WIDTH   0)
		   (HEIGHT    0) (DEPTH   0) (LEVEL   0) (SIZE   2)
		   (BREAK     0) (RIGHT   0) (LINES   1) BKPT
		   (BKPTWD    0) (BKPTHT  1) (BKPTDP  0) (BKPTOUT 0)
		   (BKPTLEVEL 0) IN-P
		   (MOREFLUSH D-MOREFLUSH)
		   MORE-^W
		   (MOREMSG D-MOREMSG))
	       (UNWIND-PROTECT
		(PROGN (SETQ FORM (DIMENSION FORM
					     NIL 'MPAREN 'MPAREN 0 0))
		       (CHECKBREAK FORM WIDTH)
		       (OUTPUT FORM (IF (AND (NOT $LEFTJUST) (= 2 LINES))
					(f- LINEL (f- WIDTH BKPTOUT))
					0))
		       (IF (AND SMART-TTY (NOT (AND SCROLLP (NOT $CURSORDISP)))
				(> (CAR (CURSORPOS)) (f- TTYHEIGHT 3)))
			   (LET (#.writefilep) (MTERPRI))))
	     ;; make sure the linearray gets cleared out.
	     (CLEAR-LINEARRAY))))
	    (T (LINEAR-DISPLA FORM)))))

(defun break-dbm-loop (at)
  (let* (
	 (*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	 (*break-level* (if (not at) *break-level* (cons t *break-level*)))
	 (*quit-tag* (cons nil nil))
	 (*break-env* *break-env*)
	 (*mread-prompt* "")
	 (*diff-bindlist* nil)
	 (*diff-mspeclist* nil)
	 val
	 )
    (declare (special *mread-prompt* ))
    (and (consp at) (set-env at))
    (cond ((null at)
	   ($frame 0 nil)))
    (catch 'step-continue
      (catch *quit-tag*
	(unwind-protect
	    (do () (())
		(format *debug-io*
		    "~@[(~a:~a) ~]"  (unless (stringp at) "dbm")
		    (length *quit-tags*))
		(setq val
		      (catch 'macsyma-quit
			(let ((res (dbm-read *debug-io*  nil *top-eof* t)))
			  (declare (special *mread-prompt*))
			  (cond ((and (consp res) (keywordp (car res)))
				 (let ((value (break-call (car res)
							  (cdr res) 'break-command)))
				   (cond ((eq value :resume) (return)))
				   ))
				(t
				 (setq $__ (nth 2 res))
				 (setq $% (meval* $__))
				 (SETQ $_ $__)
				 (displa $%)
				 ))
			  nil
			  )))
		(and (eql val 'top)
		     (throw-macsyma-top))
		      )
	 (restore-bindings)
	)))))

(DEFMFUN $ENTERMATRIX (ROWS COLUMNS)
       (PROG (ROW COLUMN VECTOR MATRIX SYM SYMVECTOR)
	     (COND ((OR (NOT (FIXNUMP ROWS))
			(NOT (FIXNUMP COLUMNS)))
		    (MERROR "ENTERMATRIX called with non-integer arguments")))
	     (SETQ ROW 0)
	     (COND ((NOT (= ROWS COLUMNS)) (SETQ SYM NIL) (GO OLOOP)))
	QUEST(PRINC "Is the matrix  1. Diagonal  2. Symmetric  3. Antisymmetric  4. General
Answer 1, 2, 3 or 4 : ")	     (SETQ SYM (RETRIEVE NIL NIL))
	     (COND ((NOT (zl-MEMBER SYM '(1 2 3 4))) (GO QUEST)))
	OLOOP(COND ((> (SETQ ROW (f1+ ROW)) ROWS)
		    (format t "~%Matrix entered.~%")
		    (RETURN (CONS '($MATRIX) (MXC MATRIX)))))
	     (COND ((EQUAL SYM 1)
		    (SETQ COLUMN ROW)
		    (PRINC "Row ") (PRINC ROW) (PRINC " Column ")
		    (PRINC COLUMN) (PRINC ":  ") 
		    (SETQ MATRIX
		     (NCONC MATRIX
		      (NCONS (ONEN ROW COLUMNS (MEVAL (RETRIEVE NIL NIL)) 0))))
		    (GO OLOOP))
		   ((EQUAL SYM 2)
		    (SETQ COLUMN (f1- ROW))
		    (COND ((EQUAL ROW 1) (GO ILOOP)))
		    (SETQ SYMVECTOR 
		           (CONS (NTHCDR COLUMN VECTOR) SYMVECTOR)
		          VECTOR (NREVERSE (MAPCAR 'CAR SYMVECTOR))
			  SYMVECTOR (MAPCAR 'CDR SYMVECTOR))
		    (GO ILOOP))
		   ((EQUAL SYM 3)
		    (SETQ COLUMN ROW)
		    (COND ((EQUAL ROW 1) (SETQ VECTOR (NCONS 0)) (GO ILOOP)))
		    (SETQ SYMVECTOR
			  (CONS (MAPCAR 'NEG
					(NTHCDR (f1- COLUMN) VECTOR))
				SYMVECTOR)
			  VECTOR (NRECONC (MAPCAR 'CAR SYMVECTOR) (NCONS 0))
			  SYMVECTOR (MAPCAR 'CDR SYMVECTOR))
		    (GO ILOOP)))	 	
	     (SETQ COLUMN 0 VECTOR NIL)
	ILOOP(COND ((> (SETQ COLUMN (f1+ COLUMN)) COLUMNS)
		    (SETQ MATRIX (NCONC MATRIX (NCONS VECTOR)))
		    (GO OLOOP)))
	     (PRINC "Row ") (PRINC ROW) (PRINC " Column ")
	     (PRINC COLUMN) (PRINC ":  ") 
	     (SETQ VECTOR (NCONC VECTOR (NCONS (MEVAL (RETRIEVE NIL NIL)))))
	     (GO ILOOP)))

(setq $display2d '$texmacs)

;; TeX-printing
;; (c) copyright 1987, Richard J. Fateman
;; Small changes for interfacing with TeXmacs: Andrey Grozin, 2001-2003

(declare-top
	 (special lop rop ccol $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))
(defconstant texport *standard-output*)

(defun tex-atom (x l r) ;; atoms: note: can we lose by leaving out {}s ?
  (append l 
	  (list (cond ((numberp x) (texnumformat x))
		      ((and (symbolp x) (get x 'texword)))
                      ((stringp x) (tex-string x))
                      ((characterp x) (tex-char x))
		      (t (tex-stripdollar x))))
	  
	  r))

(defun tex-string (x)
  (cond ((equal x "") "")
    ((eql (elt x 0) #\\) x)
    (t (concatenate 'string "\\mbox{{}" x "{}}"))))

(defun tex-char (x)
  (if (eql x #\|) "\\mbox{\\verb/|/}"
    (concatenate 'string "\\mbox{\\verb|" (string x) "|}")))

(defun tex-stripdollar (sym)
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((name (symbol-name sym))
      (pname (if (memq (elt name 0) '(#\$ #\&)) (subseq name 1) name))
      (l (length pname)))
    (cond
      ((eql l 1) pname)
      (t (concatenate 'string "\\mathrm{" pname "}")))))

(defprop mprog "\\mathbf{block}\\;" texword)

(defprop mtimes "\\*" texsym)

(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen));;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
       (cond((= (length x) 3)
	     (append l `("\\int {" ,@s1 "}{\\;d" ,@var "}") r))
	    (t ;; presumably length 5
	       (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		     ;; 1st item is 0
		     (hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
		    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;d" ,@var "}") r))))))

(defun tex-choose (x l r)
  `(,@l 
    "\\pmatrix{" 
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
    "\\\\"
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r))

(defun tex-mcond (x l r)
  (append l
    (tex (cadr x) '("\\mathbf{if}\\;")
      '("\\;\\mathbf{then}\\;") 'mparen 'mparen)
    (if (eql (fifth x) '$false)
      (tex (caddr x) nil r 'mcond rop)
      (append (tex (caddr x) nil nil 'mparen 'mparen)
        (tex (fifth x) '("\\;\\mathbf{else}\\;") r 'mcond rop)))))

(defun tex-mdo (x l r)
  (tex-list (texmdo x) l r "\\;"))

(defun tex-mdoin (x l r)
  (tex-list (texmdoin x) l r "\\;"))

(defprop mtext tex-mtext tex)
(defprop text-string tex-mtext tex)
(defprop mlable tex-mlable tex)
(defprop spaceout tex-spaceout tex)

(defun tex-mtext (x l r) (tex-list (cdr x) l r ""))

(defun tex-mlable (x l r)
  (tex (caddr x)
    (append l
      (if (cadr x)
        (list (format nil "\\mbox{\\tt\\red(~A) \\black}" (stripdollar (cadr x))))
        nil))
    r 'mparen 'mparen))

(defun tex-spaceout (x l r)
  (append l (list "\\mbox{\\verb|" (make-string (cadr x) :initial-element #\space) "|}") r))

(defun latex (x)
  (let ((ccol 1))
    (mapc #'myprinc
      (if (and (listp x) (cdr x) (equal (cadr x) "Is  "))
        (tex x '("$\\displaystyle ") '("$ ") 'mparen 'mparen)
        (tex x '("latex:$\\displaystyle ") '("$
") 'mparen 'mparen)))))
