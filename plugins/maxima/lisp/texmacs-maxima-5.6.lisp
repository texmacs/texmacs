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
;; Small changes for interfacing with TeXmacs: Andrey Grozin, 2001

(declare-top
	 (special lop rop ccol $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))
(defconstant texport t)

;;; myprinc is an intelligent low level printing routine.  it keeps track of 
;;; the size of the output for purposes of allowing the TeX file to
;;; have a reasonable line-line. myprinc will break it at a space 
;;; once it crosses a threshold.
;;; this has nothign to do with breaking the resulting equations.
 
;-      arg:    chstr -  string or number to princ
;-      scheme: This function keeps track of the current location
;-              on the line of the cursor and makes sure
;-              that a value is all printed on one line (and not divided
;-              by the crazy top level os routines)
 
(defun myprinc (chstr)
       (prog (chlst) 
              (cond ((greaterp (plus (length (setq chlst (exploden chstr)))
                                 ccol)
                           70.)
                  (terpri texport)      ;would have exceeded the line length
                      (setq ccol 1.)
		      (myprinc " ")   ; lead off with a space for safety
                      )) ;so we split it up.
             (do ((ch chlst (cdr ch))
                  (colc ccol (add1 colc)))
                 ((null ch) (setq ccol colc))
                 (tyo (car ch) texport))))

(defun myterpri nil
  (cond (texport (terpri texport))
	(t (mterpri)))
	(setq ccol 1))

(defun tex (x l r lop rop)
	;; x is the expression of interest; l is the list of strings to its
	;; left, r to its right. lop and rop are the operators on the left
	;; and right of x in the tree, and will determine if parens must
	;; be inserted
	(setq x (nformat x))
	(cond ((atom x) (tex-atom x l r))
	      ((or (<= (tex-lbp (caar x)) (tex-rbp lop)) (> (tex-lbp rop) (tex-rbp (caar x))))
	       (tex-paren x l r))
	      ;; special check needed because macsyma notates arrays peculiarly
	      ((memq 'array (cdar x)) (tex-array x l r))
	      ;; dispatch for object-oriented tex-ifiying
	      ((get (caar x) 'tex) (funcall (get (caar x) 'tex) x l r))
	      (t (tex-function x l r nil))))

(defun tex-atom (x l r) ;; atoms: note: can we lose by leaving out {}s ?
  (append l 
	  (list (cond ((numberp x) (texnumformat x))
		      ((and (symbolp x) (get x 'texword)))
                      ((stringp x) (texstring x))
                      ((characterp x) (texchar x))
		      (t (tex-stripdollar x))))
	  
	  r))

(defun texstring (x)
  (cond ((equal x "") "")
    ((eql (elt x 0) #\\) x)
    (t (concatenate 'string "\\mbox{{}" x "{}}"))))

(defun texchar (x)
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

(defun texnumformat(atom)  ;; 10/14/87 RJF  convert 1.2e20 to 1.2 \cdot 10^{20}
  (let(r firstpart exponent)
       (cond ((integerp atom)atom)
	     (t (setq r (explode atom))
		(setq exponent (memq 'e r)) ;; is it ddd.ddde+EE
		(cond ((null exponent) atom); it is not. go with it as given
		      (t (setq firstpart (nreverse (cdr (memq 'e (reverse r)))))
			 (strcat (apply #'strcat firstpart )
					 "\\cdot 10^{"
					 (apply #'strcat (cdr exponent))
					 "}")))))))

(defun tex-paren (x l r) 
  (tex x (append l '("\\left(")) (cons "\\right)" r) 'mparen 'mparen))

(defun tex-array (x l r)
  (let ((f))
       (if (eq 'mqapply (caar x))
	   (setq f (cadr x) 
		 x (cdr x))
	   (setq f (caar x)))
       (setq l (tex (texword f) l nil lop 'mfunction)
	     
	     r (nconc (tex-list (cdr x) nil (list "}") ",") r)) 
       (nconc l (list "_{") r  )))

;; we could patch this so sin x rather than sin(x), but instead we made sin a prefix
;; operator

(defun tex-function (x l r op) op
	(setq l (tex (texword (caar x)) l nil 'mparen 'mparen)
	      r (tex (cons '(mprogn) (cdr x)) nil r 'mparen 'mparen))
	(nconc l r))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"

(defun tex-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (tex (car x)  l r 'mparen 'mparen)))
	   nl)
	  (setq nl (nconc nl (tex (car x)  l (list sym) 'mparen 'mparen))
		  x (cdr x) 
		  l nil))))

(defun tex-prefix (x l r)
  (tex (cadr x) (append l (texsym (caar x))) r (caar x) rop))

(defun tex-infix (x l r)
  ;; check for 2 args
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) l nil lop (caar x)))
  (tex (caddr x) (append l (texsym (caar x))) r (caar x) rop))
  
(defun tex-postfix (x l r)
  (tex (cadr x) l (append (texsym (caar x)) r) lop (caar x)))

(defun tex-nary (x l r)
  (let* ((op (caar x)) (sym (texsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (tex-function x l r t)) ; this should not happen
          ((null (cdr y)) (tex-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (nconc nl (tex (car y)  l r lop rop))) nl)
	         (setq nl (nconc nl (tex (car y)  l (list sym)   lop rop))
		       y (cdr y) 
		       l nil))))))

(defun tex-nofix (x l r) (tex (caar x) l r (caar x) rop))

(defun tex-matchfix (x l r)
  (setq l (append l (car (texsym (caar x))))
	;; car of texsym of a matchfix operator is the lead op
	r (append (cdr (texsym (caar x))) r) 
	;; cdr is the trailing op
	x (tex-list (cdr x) nil r ","))
  (append l x))

(defun texsym (x) (or (get x 'texsym) (get x 'strsym)(get x 'dissym)
		      (stripdollar x)))

(defun texword (x)(or (get x 'texword) (stripdollar x)))

(defprop bigfloat tex-bigfloat tex)

(defun tex-bigfloat (x l r) (fpformat x))

(defprop mprog "\\mathbf{block}\\;" texword)
(defprop %erf "\\mathrm{erf}" texword)
(defprop $erf "\\mathrm{erf}" texword) ;; etc for multicharacter names
(defprop $true  "\\mathbf{true}"  texword)
(defprop $false "\\mathbf{false}" texword)

(defprop mprogn tex-matchfix tex) ;; mprogn is (<progstmnt>, ...)
(defprop mprogn (("\\left(") "\\right)") texsym)

(defprop mlist tex-matchfix tex)
(defprop mlist (("\\left[ ")" \\right] ") texsym)

;;absolute value
(defprop mabs tex-matchfix tex)
(defprop mabs (("\\left| ")"\\right| ") texsym)

(defprop mqapply tex-mqapply tex)

(defun tex-mqapply (x l r)
  (setq l (tex (cadr x) l (list "(" ) lop 'mfunction)
	r (tex-list (cddr x) nil (cons ")" r) ","))
  (append l r));; fixed 9/24/87 RJF

(defprop $%i "i" texword)
(defprop $%pi "\\pi" texword)
(defprop $%e "e" texword)
(defprop $inf "\\infty " texword)
(defprop $minf " -\\infty " texword)
(defprop %laplace "{\\cal L}" texword)
(defprop $alpha "\\alpha" texword)
(defprop $beta "\\beta" texword)
(defprop $gamma "\\gamma" texword)
(defprop %gamma "\\Gamma" texword)
(defprop $%gamma "\\gamma" texword)
(defprop $delta "\\delta" texword)
(defprop $epsilon "\\varepsilon" texword)
(defprop $zeta "\\zeta" texword)
(defprop $eta "\\eta" texword)
(defprop $theta "\\vartheta" texword)
(defprop $iota "\\iota" texword)
(defprop $kappa "\\varkappa" texword)
;(defprop $lambda "\\lambda" texword)
(defprop $mu "\\mu" texword)
(defprop $nu "\\nu" texword)
(defprop $xi "\\xi" texword)
(defprop $pi "\\pi" texword)
(defprop $rho "\\rho" texword)
(defprop $sigma "\\sigma" texword)
(defprop $tau "\\tau" texword)
(defprop $upsilon "\\upsilon" texword)
(defprop $phi "\\varphi" texword)
(defprop $chi "\\chi" texword)
(defprop $psi "\\psi" texword)
(defprop $omega "\\omega" texword)

(defprop mquote tex-prefix tex)
(defprop mquote ("'") texsym)
(defprop mquote 201. tex-rbp)

(defprop msetq tex-infix tex)
(defprop msetq (":") texsym)
(defprop msetq 180. tex-rbp)
(defprop msetq 20. tex-rbp)

(defprop mset tex-infix tex)
(defprop mset ("::") texsym)
(defprop mset 180. tex-lbp)
(defprop mset 20. tex-rbp)

(defprop mdefine tex-infix tex)
(defprop mdefine (":=") texsym)
(defprop mdefine 180. tex-lbp)
(defprop mdefine 20. tex-rbp)

(defprop mdefmacro tex-infix tex)
(defprop mdefmacro ("::=") texsym)
(defprop mdefmacro 180. tex-lbp)
(defprop mdefmacro 20. tex-rbp)

(defprop marrow tex-infix tex)
(defprop marrow ("\\rightarrow ") texsym)
(defprop marrow 25 tex-lbp)
(defprop marrow 25 tex-rbp)

(defprop mfactorial tex-postfix tex)
(defprop mfactorial ("!") texsym)
(defprop mfactorial 160. tex-lbp)

(defprop mexpt tex-mexpt tex)
(defprop mexpt 140. tex-lbp)
(defprop mexpt 139. tex-rbp)

;; insert left-angle-brackets for mncexpt. a^<n> is how a^^n looks.
(defun tex-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt))); true if a^^b rather than a^b
     ;; here is where we have to check for f(x)^b to be displayed
     ;; as f^b(x), as is the case for sin(x)^2 .
     ;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2. 
     ;; yet we must not display (a+b)^2 as +^2(a,b)...
     ;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
     (cond ;; this whole clause
	   ;; should be deleted if this hack is unwanted and/or the
	   ;; time it takes is of concern.
	   ;; it shouldn't be too expensive.
	   ((and (eq (caar x) 'mexpt) ; don't do this hack for mncexpt
		 (let* 
		  ((fx (cadr x)); this is f(x)
		   (f (and (not (atom fx)) (atom (caar fx)) (caar fx))) ; this is f [or nil]
		   (bascdr (and f (cdr fx))) ; this is (x) [maybe (x,y..), or nil]
		   (expon (caddr x)) ;; this is the exponent
		   (doit (and 
			  f ; there is such a function
			  (memq (getchar f 1) '(% $)) ;; insist it is a % or $ function
			  (not (memq f '(%sum %product))) ;; what else? what a hack...
			  (or (and (atom expon) (not (numberp expon))) ; f(x)^y is ok
			      (and (atom expon) (numberp expon) (> expon 0))))))
			      ; f(x)^3 is ok, but not f(x)^-1, which could 
			      ; inverse of f, if written f^-1 x
			      ; what else? f(x)^(1/2) is sqrt(f(x)), ??
		  (cond (doit
			(setq l (tex `((mexpt) ,f ,expon) l nil 'mparen 'mparen))
			(setq r (tex
                                 (if (and (null (cdr bascdr)) (eq (get f 'tex) 'tex-prefix))
                                     (car bascdr) (cons '(mprogn) bascdr))
                                 nil r f rop)))
		        (t nil))))) ; won't doit. fall through
      (t (setq l (tex (cadr x) l nil lop (caar x))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		    ;; the change in base-line makes parens unnecessary
		    (if nc
			(tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
			(tex (cadr x) '("^ {- ")(cons " }" r) 'mparen 'mparen))
		    (if nc
			(tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
			(tex x (list "^{")(cons "}" r) 'mparen 'mparen))))))
      (append l r)))

(defprop mncexpt tex-mexpt tex)

(defprop mncexpt 135. tex-lbp)
(defprop mncexpt 134. tex-rbp)

(defprop mnctimes tex-nary tex)
(defprop mnctimes "\\cdot " texsym)
(defprop mnctimes 110. tex-lbp)
(defprop mnctimes 109. tex-rbp)

(defprop mtimes tex-nary tex)
(defprop mtimes "\\*" texsym)
(defprop mtimes 120. tex-lbp)
(defprop mtimes 120. tex-rbp)

(defprop %sqrt tex-sqrt tex)

(defun tex-sqrt(x l r)
  ;; format as \\sqrt { } assuming implicit parens for sqr grouping
  (tex (cadr x) (append l  '("\\sqrt{")) (append '("}") r) 'mparen 'mparen))

;; macsyma doesn't know about cube (or nth) roots,
;; but if it did, this is what it would look like.
(defprop $cubrt tex-cubrt tex)

(defun tex-cubrt (x l r)
  (tex (cadr x) (append l  '("\\root 3 \\of{")) (append '("}") r) 'mparen 'mparen))

(defprop mquotient tex-mquotient tex)
(defprop mquotient ("\\over") texsym)
(defprop mquotient 122. tex-lbp) ;;dunno about this
(defprop mquotient 123. tex-rbp) 

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) (append l '("{{")) nil 'mparen 'mparen)
	;the divide bar groups things
	r (tex (caddr x) (list "}\\over{") (append '("}}")r) 'mparen 'mparen))
  (append l r))

(defprop $matrix tex-matrix tex)

(defun tex-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("\\pmatrix{")
	 (mapcan #'(lambda(y)
			  (tex-list (cdr y) nil (list "\\cr ") "&")) 
		 (cdr x))
	 '("}") r))

;; macsyma sum or prod is over integer range, not  low <= index <= high
;; TeX is lots more flexible .. but

(defprop %sum tex-sum tex)
(defprop %lsum tex-lsum tex)
(defprop %product tex-sum tex)

;; easily extended to union, intersect, otherops

(defun tex-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "}}{" ,@s1 "}") r)))

(defun tex-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "\\sum_{")
		  ((eq (caar x) '%product) "\\prod_{")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (tex `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (tex (car(cddddr x)) nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}") r)))

(defprop %integrate tex-int tex)
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

(defprop %limit tex-limit tex)

(defun tex-limit(x l r) ;; ignoring direction, last optional arg to limit
  (let ((s1 (tex (cadr x) nil nil 'mparen rop));; limitfunction
	(subfun ;; the thing underneath "limit"
	 (subst "\\rightarrow " '=
		(tex `((mequal simp) ,(caddr x),(cadddr x))
		     nil nil 'mparen 'mparen))))
       (append l `("\\lim_{" ,@subfun "}{" ,@s1 "}") r)))

(defprop %at tex-at tex)

;; e.g.  at(diff(f(x)),x=a)
(defun tex-at (x l r)
  (let ((s1 (tex (cadr x) nil nil lop rop))
	(sub (tex (caddr x) nil nil 'mparen 'mparen)))
       (append l '("\\left.") s1  '("\\right|_{") sub '("}") r)))

;(defprop mbox tex-mbox tex)
;
;(defun tex-mbox (x l r)
;  (append l '("\\framebox{") (tex (cadr x) nil nil 'mparen 'mparen) '("}")))

;;binomial coefficients

(defprop %binomial tex-choose tex)
	   
(defun tex-choose (x l r)
  `(,@l 
    "\\pmatrix{" 
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
    "\\\\"
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r))


(defprop rat tex-rat tex) 
(defprop rat 120. tex-lbp)
(defprop rat 121. tex-rbp)
(defun tex-rat(x l r) (tex-mquotient x l r))

(defprop mplus tex-mplus tex)
(defprop mplus 100. tex-lbp)
(defprop mplus 100. tex-rbp)

(defun tex-mplus (x l r)
 ;(declare (fixnum w))
 (cond ((memq 'trunc (car x))(setq r (cons "+\\cdots " r))))
 (cond ((null (cddr x))
	(if (null (cdr x))
	    (tex-function x l r t)
	    (tex (cadr x) (cons "+" l) r 'mplus rop)))
       (t (setq l (tex (cadr x) l nil lop 'mplus) 
		x (cddr x))
	  (do ((nl l)  (dissym))
	      ((null (cdr x))
	       (if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
		   (setq l (car x) dissym (list "+")))
	       (setq r (tex l dissym r 'mplus rop))
	       (append nl r))
	      (if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
		  (setq l (car x) dissym (list "+")))
	      (setq nl (append nl (tex l dissym nil 'mplus 'mplus))
		    x (cdr x))))))

(defprop mminus tex-prefix tex)
(defprop mminus ("-") texsym)
(defprop mminus 100. tex-rbp)
(defprop mminus 100. tex-lbp)

(defprop min tex-infix tex)
(defprop min ("\\in{") texsym)
(defprop min 80. tex-lbp)
(defprop min 80. tex-rbp)

(defprop mequal tex-infix tex)
(defprop mequal (=) texsym)
(defprop mequal 80. tex-lbp)
(defprop mequal 80. tex-rbp)

(defprop mnotequal tex-infix tex)
(defprop mnotequal 80. tex-lbp)
(defprop mnotequal 80. tex-rbp)

(defprop mgreaterp tex-infix tex)
(defprop mgreaterp (>) texsym)
(defprop mgreaterp 80. tex-lbp)
(defprop mgreaterp 80. tex-rbp)

(defprop mgeqp tex-infix tex)
(defprop mgeqp ("\\geq") texsym)
(defprop mgeqp 80. tex-lbp)
(defprop mgeqp 80. tex-rbp)

(defprop mlessp tex-infix tex)
(defprop mlessp (<) texsym)
(defprop mlessp 80. tex-lbp)
(defprop mlessp 80. tex-rbp)

(defprop mleqp tex-infix tex)
(defprop mleqp ("\\leq") texsym)
(defprop mleqp 80. tex-lbp)
(defprop mleqp 80. tex-rbp)

(defprop mnot tex-prefix tex)
(defprop mnot ("\\not ") texsym)
(defprop mnot 70. tex-rbp)

(defprop mand tex-nary tex)
(defprop mand ("\\and") texsym)
(defprop mand 60. tex-lbp)
(defprop mand 60. tex-rbp)

(defprop mor tex-nary tex)
(defprop mor ("\\or") texsym)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(defun tex-setup (x)
  (let((a (car x))
       (b (cadr x)))
      (setf (get a 'tex) 'tex-prefix)
      (setf (get a 'texword) b)  ;This means "sin" will always be roman
      (setf (get a 'texsym) (list b))
      (setf (get a 'tex-rbp) 130)))

(mapc #'tex-setup 
  '( (%sin "\\sin ")
     (%cos "\\cos ")
     (%tan "\\tan ")
     (%cot "\\cot ")
     (%sec "\\sec ")
     (%csc "\\csc ")
     (%asin "\\arcsin ")
     (%acos "\\arccos ")
     (%atan "\\arctan ")
     (%sinh "\\sinh ")
     (%cosh "\\cosh ")
     (%tanh "\\tanh ")
     (%coth "\\coth ")
     (%ln "\\ln ")
     (%log "\\log ")
    ;; (%erf "{\\rm erf}") this would tend to set erf(x) as erf x. Unusual
     ;(%laplace "{\\cal L}")
     )) ;; etc

(defprop mor tex-nary tex)
(defprop mor 50. tex-lbp)
(defprop mor 50. tex-rbp)

(defprop mcond tex-mcond tex)
(defprop mcond 25. tex-lbp)
(defprop mcond 25. tex-rbp)
(defprop %derivative tex-derivative tex)
(defun tex-derivative (x l r)
  (tex (tex-d x '$|d|) l r lop rop ))

(defun tex-d(x dsym) ;dsym should be $d or "$d\\partial"
  ;; format the macsyma derivative form so it looks
  ;; sort of like a quotient times the deriva-dand.
  (let*
   ((arg (cadr x)) ;; the function being differentiated
    (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
    (ords (odds difflist 0)) ;; e.g. (1 2)
    (vars (odds difflist 1)) ;; e.g. (x y)
    (numer `((mexpt) $|d| ((mplus) ,@ords))) ; d^n numerator
    (denom (cons '(mtimes)
		 (mapcan #'(lambda(b e)
				  `(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))
			 vars ords))))
   `((mtimes)
     ((mquotient) ,(simplifya numer nil) ,denom)
     ,arg)))

(defun odds(n c) 
  ;; if c=1, get the odd terms  (first, third...)
  (cond ((null n) nil) 
	((= c 1)(cons (car n)(odds (cdr n) 0)))
	((= c 0)(odds (cdr n) 1))))

(defun tex-mcond (x l r)
  (append l
    (tex (cadr x) '("\\mathbf{if}\\;")
      '("\\;\\mathbf{then}\\;") 'mparen 'mparen)
    (if (eql (fifth x) '$false)
      (tex (caddr x) nil r 'mcond rop)
      (append (tex (caddr x) nil nil 'mparen 'mparen)
        (tex (fifth x) '("\\;\\mathbf{else}\\;") r 'mcond rop)))))

(defprop mdo tex-mdo tex)
(defprop mdo 30. tex-lbp)
(defprop mdo 30. tex-rbp)
(defprop mdoin tex-mdoin tex)
(defprop mdoin 30. tex-rbp)

(defun tex-lbp(x)(cond((get x 'tex-lbp))(t(lbp x))))
(defun tex-rbp(x)(cond((get x 'tex-rbp))(t(lbp x))))

;; these aren't quite right

(defun tex-mdo (x l r)
  (tex-list (texmdo x) l r "\\;"))

(defun tex-mdoin (x l r)
  (tex-list (texmdoin x) l r "\\;"))

(defun texmdo (x)
   (nconc (cond ((second x) `("\\mathbf{for}" ,(second x))))
	 (cond ((equal 1 (third x)) nil)
	       ((third x)  `("\\mathbf{from}" ,(third x))))
	 (cond ((equal 1 (fourth x)) nil)
	       ((fourth x) `("\\mathbf{step}" ,(fourth x)))
	       ((fifth x)  `("\\mathbf{next}" ,(fifth x))))
	 (cond ((sixth x)  `("\\mathbf{thru}" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("\\mathbf{while}" ,(cadr (seventh x))))
	       (t `("\\mathbf{unless}" ,(seventh x))))
	 `("\\mathbf{do}" ,(eighth x))))

(defun texmdoin (x)
  (nconc `("\\mathbf{for}" ,(second x) $|in| ,(third x))
	 (cond ((sixth x) `("\\mathbf{thru}" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("\\mathbf{while}" ,(cadr (seventh x))))
	       (t `("\\mathbf{unless}" ,(seventh x))))
	 `("\\mathbf{do}" ,(eighth x))))


;; Undone and trickier:
;; handle reserved symbols stuff, just in case someone
;; has a macsyma variable named (yuck!!) \over  or has a name with 
;; {} in it.
;; Maybe do some special hacking for standard notations for 
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.

;;Undone and really pretty hard: line breaking

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
