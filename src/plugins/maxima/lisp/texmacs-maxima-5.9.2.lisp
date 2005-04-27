(in-package "MAXIMA")
#+clisp (defvar *old-suppress-check-redefinition* 
	      custom:*suppress-check-redefinition*)
#+clisp (setf custom:*suppress-check-redefinition* t)
(setf *alt-display2d* 'texmacs)
(setf *prompt-prefix* "channel:promptlatex:\\red ")
(setf *prompt-suffix* "\\black")
(setf *general-display-prefix* "verbatim:")
(setf *maxima-prolog* "verbatim:")
(setf *maxima-epilog* "latex:\\red The end\\black")
#-gcl(setf *debug-io* (make-two-way-stream *standard-input* *standard-output*))

;; Small changes to mactex.lisp for interfacing with TeXmacs
;; Andrey Grozin, 2001-2005

(defun main-prompt ()
  (format () "~A(~A~D) ~A" *prompt-prefix* 
    (tex-stripdollar $inchar) $linenum *prompt-suffix*))

(declare-top
	 (special lop rop ccol $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))
(defconstant texport *standard-output*)

(defun tex-stripdollar (sym)
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((name (quote-% (print-invert-case sym)))
      (name1 (if (memq (elt name 0) '(#\$ #\&)) (subseq name 1) name))
      (l (length name1)))
    (if (eql l 1) name1 (concatenate 'string "\\mathrm{" name1 "}"))))

(defprop mtimes "\\*" texsym)

(defun texmacs (x)
  (let ((ccol 1))
    (mapc #'myprinc
        (tex x '("latex:$\\displaystyle ") '("$
") 'mparen 'mparen))))

;; In order to allow cut-and-paste from output to input,
;; we should output \sin(x), not \sin x.

(map 'list #'(lambda (f) (remprop f 'tex) (remprop f 'tex-rbp))
  '(%sin %cos %tan %cot %sec %csc %sinh %cosh %tanh %coth %asin %acos %atan %exp %log))

(remprop '$pi 'texword)
(remprop '$gamma 'texword)
(setf (get '$%i 'texword) "\\mathi")
(setf (get '$%e 'texword) "\\mathe")

;; Also, we should output f(x)^2, not f^2(x)

(defun tex-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt)))	; true if a^^b rather than a^b
    (setq l (if (and (numberp (cadr x)) (numneedsparen (cadr x)))
                (tex (cadr x) (cons "\\left(" l) '("\\right)") lop (caar x))
		(tex (cadr x) l nil lop (caar x)))
          r (if (mmminusp (setq x (nformat (caddr x))))
		;; the change in base-line makes parens unnecessary
		(if nc
		    (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
		    (tex (cadr x) '("^ {- ")(cons " }" r) 'mparen 'mparen))
		(if nc
		    (tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
		    (if (and (integerp x) (< x 10))
			(tex x (list "^")(cons "" r) 'mparen 'mparen)
			(tex x (list "^{")(cons "}" r) 'mparen 'mparen)))))
    (append l r)))

;; binomial coefficients

(defun tex-choose (x l r)
  `(,@l
    "\\binom{"
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
    "}{"
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r))

;; Integrals, sums, products

(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen)) ;;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
    (cond((= (length x) 3)
	  (append l `("\\int {" ,@s1 "}{\\;\\mathd\\;" ,@var "}\\big.") r))
	 (t ;; presumably length 5
	  (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		;; 1st item is 0
		(hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
	    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;\\mathd\\;" ,@var "}\\big.") r))))))

(defun tex-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "\\sum_{")
		  ((eq (caar x) '%product) "\\prod_{")
		  ;; extend here
		  ))
	;; gotta be one of those above
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (tex (car(cddddr x)) nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}\\big.") r)))

(defun tex-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}}{" ,@s1 "}\\big.") r)))

;; This is a hack for math input of integrals, sums, products

(defmfun $tmint (a b f x) ($integrate f x a b))

(defmspec $tmsum (l) (setq l (cdr l))
  (if (= (length l) 3)
      (dosum (caddr l) (cadar l) (meval (caddar l)) (meval (cadr l)) t)
      (wna-err '$tmsum)))

(defmspec $tmlsum (l) (setq l (cdr l))
  (or (= (length l) 2) (wna-err '$tmlsum))
  (let ((form (cadr l))
        (ind (cadar l))
        (lis (meval (caddar l)))
        (ans 0))
       (or (symbolp ind) (merror "Second argument not a variable ~M" ind))
       (cond (($listp lis)
              (loop for v in (cdr lis)
                    with lind = (cons ind nil)
                    for w = (cons v nil)
                    do
                    (setq ans (add* ans  (mbinding (lind w) (meval form)))))
                   ans)
           (t `((%lsum) ,form ,ind ,lis)))))

(defmspec $tmprod (l) (setq l (cdr l))
  (if (= (length l) 3)
      (dosum (caddr l) (cadar l) (meval (caddar l)) (meval (cadr l)) nil)
      (wna-err '$tmprod)))

#+clisp (setf custom:*suppress-check-redefinition*
	      *old-suppress-check-redefinition*)
