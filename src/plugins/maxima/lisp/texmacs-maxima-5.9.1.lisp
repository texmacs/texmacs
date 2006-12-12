(in-package :maxima)
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
;; Andrey Grozin, 2001-2004

(defun main-prompt ()
  (format () "~A(~A~D) ~A" *prompt-prefix* 
    (tex-stripdollar $inchar) $linenum *prompt-suffix*))

(declare-top
	 (special lop rop ccol $gcprint $inchar)
	 (*expr tex-lbp tex-rbp))
(defconstant texport *standard-output*)

(defun tex-stripdollar (sym)
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((name (symbol-name sym))
      (name1 (if (memq (elt name 0) '(#\$ #\&)) (subseq name 1) name))
      (name2 (if (eql (elt name1 0) #\%) (concatenate 'string "\\" name1) name1))
      (l (length name2)))
    (if (eql l 1) name2 (concatenate 'string "\\mathrm{" name2 "}"))))

(defprop mtimes "\\*" texsym)

(defun texmacs (x)
  (let ((ccol 1))
    (mapc #'myprinc
        (tex x '("latex:$\\displaystyle ") '("$
") 'mparen 'mparen))))

#+clisp (setf custom:*suppress-check-redefinition*
	      *old-suppress-check-redefinition*)
