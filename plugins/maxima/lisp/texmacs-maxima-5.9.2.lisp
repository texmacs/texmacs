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

#+clisp (setf custom:*suppress-check-redefinition*
	      *old-suppress-check-redefinition*)
