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

(defprop $matrix tex-matrix tex)

(defun tex-matrix (x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("\\pmatrix{")
          (mapcan #'(lambda(y)
                      (tex-list (cdr y) nil (list "\\cr ") "&"))
                  (cdr x))
          '("}") r))

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

;; delete a previous temporary file (ps or eps)
(let ((p-path) (pname))
  (defun tm-delete-file (path)
    (cond ((null p-path)
           (setq p-path path))
          ((not (string= p-path path))
           (setq pname (probe-file p-path))
           (if pname (delete-file pname))
           (setq p-path path)))))

;; create an inline figure, based on raw ps code
(defun tm-out (raw_data)
 (let ((beg (string (code-char 2)))
        (end (string (code-char 5))))
   (concatenate 'string beg "ps:" raw_data end)))

;; create an inline figure, based on a .ps/.eps filename
(defmfun $ps_out (filename)
  ;; wait for the completion of the ps file
  (let ((len1) (len2 -1))
    (with-open-file (stream filename)
                    (setq len1 (file-length stream)))
    (loop while (/= len1 len2)
          do (sleep 0.1)
          (setq len2 len1)
          (with-open-file (stream filename)
                          (setq len1 (file-length stream)))))
  ;; delete a previous temporary file
  (tm-delete-file filename)
  ;; create an inline figure
  (with-open-file (stream filename)
                  (let ((contents (make-string (file-length stream))))
                    (read-sequence contents stream)
                    ;; princ does not enclose the string in quotes
                    (princ (tm-out contents))
                    (princ ""))))

;; a predefined interval of time
(defvar tm-time-out 10.0)  ; 10.0 second

;; make a name of a temporary file
(defun make-tm-temp-name ()
  (string (gensym (format nil "tm_temp_plot_~A_" (getpid)))))

;; get a float value of a internal real time
(defun get-tm-internal-real-time ()
  (float (/ (get-internal-real-time) INTERNAL-TIME-UNITS-PER-SECOND)))

;; create an inline figure with a ps file
(defun tm-plot (fun args)
  (block nil
    (let ((tmp-name (concatenate 'string (make-tm-temp-name) ".ps"))
          (d-time 0.0)
          (s-time (get-tm-internal-real-time))
          (p-format ($get_plot_option '$plot_format))
          (gp-term ($get_plot_option '$gnuplot_term))
          (gp-file ($get_plot_option '$gnuplot_out_file))
          (a-win) (ret))
      (if (boundp '*autoconf-windows*)
          (setq a-win (symbol-value '*autoconf-windows*))
        (setq a-win (symbol-value '*autoconf-win32*)))
      (if (null gp-term)
          (setq gp-term (append '((mlist) $gnuplot_term)
                                (list (getf *plot-options* :GNUPLOT_TERM)))))
      ;; set necessary options
      ($set_plot_option (append '((mlist) $plot_format)
                                (list (if (string= a-win "true")
                                          '$gnuplot
                                        '$gnuplot_pipes))))
      ($set_plot_option '((mlist) $gnuplot_term $ps))
      ($set_plot_option (append '((mlist) $gnuplot_out_file) (list tmp-name)))
      ;; create a ps file
      (setq ret (meval1 (cons '$errcatch
                             (list (cons fun
                                         (remove-if #'(lambda (x) ; remove unnecessary options if they exist
                                                        (and (listp x)
                                                             (listp (car x)) (eq 'mlist (caar x))
                                                             (listp (cdr x)) (member (cadr x)
                                                                                     '($plot_format
                                                                                       $gnuplot_term
                                                                                       $gnuplot_out_file))))
                                                    args))))))
      (if gp-file                       ; restore $gnuplot_out_file
          ($set_plot_option gp-file)
        ($remove_plot_option '$gnuplot_out_file))
      ($set_plot_option gp-term)        ; restore $gnuplot_term
      ($set_plot_option p-format)       ; restore $plot_format
      ;; return if an error occurs
      (if ($emptyp ret) (return-from nil)) 
      ;; try to read the ps file again if it does not exist
      (tagbody
       start
       (setq d-time (- (get-tm-internal-real-time) s-time))
       (handler-bind ((file-error #'(lambda (condition)
                                      (cond ((> d-time tm-time-out)
                                             (format t "~A~%" condition)
                                             (go finish))
                                            (t
                                             (sleep 0.1)
                                             (go start))))))
         (funcall '$ps_out (plot-file-path tmp-name)))
       (return-from nil t)
       finish))))

;; same as plot2d, but also create an inline figure
(defmfun $tm_plot2d (&rest args)
  (tm-plot '$plot2d args))

;; same as plot3d, but also create an inline figure
(defmfun $tm_plot3d (&rest args)
  (tm-plot '$plot3d args))

;; same as contour_plot, but also create an inline figure
(defmfun $tm_contour_plot (&rest args)
  (tm-plot '$contour_plot args))

;; same as implicit_plot, but also create an inline figure
(defmfun $tm_implicit_plot (&rest args)
  (let ((fun '$implicit_plot))
    (if (not (functionp fun))
        ($load "implicit_plot"))
    (tm-plot fun args)))

;; same as julia, but also create an inline figure
(defmfun $tm_julia (&rest args)
  (let ((fun '$julia))
    (if (not (functionp fun))
        ($load "dynamics"))
    (tm-plot fun args)))

;; same as mandelbrot, but also create an inline figure
(defmfun $tm_mandelbrot (&rest args)
  (let ((fun '$mandelbrot))
    (if (not (functionp fun))
        ($load "dynamics"))
    (tm-plot fun args)))

;; remove unnecessary equations ($terminal and $file_name) if they exist
(defun get-tm-draw-args (args)
  (remove-if #'(lambda (x)
                 (and (listp x)
                      (listp (car x)) (eq 'mequal (caar x))
                      (member (cadr x)
                              '($terminal $file_name))))
             args))

;; create an inline figure with a eps file
(defun tm-draw (fun args)
  (block nil
    (let ((tmp-name (plot-file-path (make-tm-temp-name)))
          (d-time 0.0)
          (s-time (get-tm-internal-real-time))
          (options nil)
          (renderer nil)
          (ret))
      (if (not (boundp '*user-gr-default-options*))
          ($load "draw"))
      ;; save options
      (setq options (copy-list (symbol-value '*user-gr-default-options*)))
      ;; save renderer
      (setq renderer (symbol-value '$draw_renderer))
      ;; set renderer
      (setf (symbol-value '$draw_renderer) '$gnuplot_pipes)
      ;; set necessary options
      (apply '$set_draw_defaults
             (append (get-tm-draw-args options)
                     (list '((mequal) $terminal $eps_color)
                           (append '((mequal) $file_name) (list tmp-name)))))
      ;; create a eps file
      (setq ret (meval1 (cons '$errcatch (list (cons fun (get-tm-draw-args args))))))
      ;; restore options
      (setf (symbol-value '*user-gr-default-options*) options)
      ;; restore renderer
      (setf (symbol-value '$draw_renderer) renderer)
      ;; return if an error occurs
      (if ($emptyp ret) (return-from nil))
      ;; try to read the eps file again if it does not exist
      (tagbody
       start
       (setq d-time (- (get-tm-internal-real-time) s-time))
       (handler-bind ((file-error #'(lambda (condition)
                                      (cond ((> d-time tm-time-out)
                                             (format t "~A~%" condition)
                                             (go finish))
                                            (t
                                             (sleep 0.1)
                                             (go start))))))
         (funcall '$ps_out (concatenate 'string tmp-name ".eps")))
       (return-from nil t)
       finish))))

;; same as draw, but also create an inline figure
(defmfun $tm_draw (&rest args)
  (tm-draw '$draw args))

;; same as draw2d, but also create an inline figure
(defmfun $tm_draw2d (&rest args)
  (tm-draw '$draw2d args))

;; same as draw3d, but also create an inline figure
(defmfun $tm_draw3d (&rest args)
  (tm-draw '$draw3d args))
