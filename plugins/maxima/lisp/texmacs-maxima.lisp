(in-package :maxima)

;; borrowed from wxmaxima by Andrej Vodopivec
;; and a patch proposed by John Lapeyre
(defmacro texmacs-no-warning (form)
  #+sbcl `(handler-bind
            ((style-warning #'muffle-warning)
              (sb-ext:compiler-note #'muffle-warning))
          ,form)
  #+clisp `(let ((custom:*suppress-check-redefinition* t)) ,form)
  #-(or sbcl clisp) `(progn ,form))

(setf *alt-display2d* 'texmacs)
(setf *prompt-prefix* "channel:promptlatex:\\red ")
(setf *prompt-suffix* "\\black")
;(setf *general-display-prefix* "verbatim:")
(setf *maxima-prolog* "verbatim:")
(setf *maxima-epilog* "latex:\\red The end\\black")
#-gcl(setf *debug-io* (make-two-way-stream *standard-input* *standard-output*))
#+(or cmu sbcl scl)
(setf *terminal-io* (make-two-way-stream *standard-input* *standard-output*))

;; Small changes to mactex.lisp for interfacing with TeXmacs
;; Andrey Grozin, 2001-2006

(texmacs-no-warning
(defun main-prompt ()
  (format () "~A(~A~D) ~A" *prompt-prefix* 
          (tex-stripdollar $inchar) $linenum *prompt-suffix*)))

(declare-top
	 (special lop rop ccol $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))
(defconstant texport *standard-output*)

(texmacs-no-warning
(defun tex-stripdollar (x)
  (let ((s (quote-% (maybe-invert-string-case (symbol-name (stripdollar x))))))
    (if (> (length s) 1)
        (concatenate 'string "\\mathrm{" s "}")
      s))))

(defprop mtimes ("\\*") texsym)

(defun texmacs (x)
  (let ((ccol 1))
    ;;(mapc #'(lambda (y) (myprinc y t))
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

(texmacs-no-warning
(defun tex-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt)))	; true if a^^b rather than a^b
    (setq l (if (and (numberp (cadr x)) (numneedsparen (cadr x)))
                (tex (cadr x) (cons "\\left(" l) '("\\right)") lop (caar x))
              (tex (cadr x) l nil lop (caar x)))
          r (if (mmminusp (setq x (nformat (caddr x))))
                ;; the change in base-line makes parens unnecessary
                (if nc
                    (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
                  (tex (cadr x) '("^ {- ")(cons " }" r) 'mminus 'mparen))
              (if nc
                  (tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
                (if (and (integerp x) (< x 10))
                    (tex x (list "^")(cons "" r) 'mparen 'mparen)
                  (tex x (list "^{")(cons "}" r) 'mparen 'mparen)))))
    (append l r))))

;; binomial coefficients

(texmacs-no-warning
(defun tex-choose (x l r)
  `(,@l
    "\\binom{"
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
    "}{"
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r)))

;; Integrals, sums, products

(texmacs-no-warning
(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen)) ;;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
    (cond((= (length x) 3)
	  (append l `("\\int {" ,@s1 "}{\\;\\mathd\\;" ,@var "}\\big.") r))
	 (t ;; presumably length 5
	  (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		;; 1st item is 0
		(hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
	    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;\\mathd\\;" ,@var "}\\big.") r)))))))

(texmacs-no-warning
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
    (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}\\big.") r))))

(texmacs-no-warning
(defun tex-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}}{" ,@s1 "}\\big.") r))))

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

;; == Inline plots

;; create an inline figure, based on raw ps code
(defun tm_out (raw_data)
 (let ((beg (string (code-char 2)))
        (end (string (code-char 5))))
   (concatenate 'string beg "ps:" raw_data end)))

;; create an inline figure, based on a .ps filename
(defmfun $ps_out (filename)
  (with-open-file (stream filename)
                  (let ((contents (make-string (file-length stream))))
                    (read-sequence contents stream)
                    ;; princ does not enclose the string in quotes
                    (princ (tm_out contents))
                    (princ ""))))

;; same as plot2d, but also create an inline figure
(defmfun $tm_plot2d (&rest args)
  #$set_plot_option([gnuplot_term, ps])$
  #$set_plot_option([gnuplot_out_file, "tm_temp_plot.ps"])$
  (apply '$plot2d args)
  #$remove_plot_option(gnuplot_out_file)$
  #$set_plot_option([gnuplot_term, default])$
  (apply '$plot2d args)
  (funcall '$ps_out (concatenate 'string $maxima_tempdir "/tm_temp_plot.ps")))
