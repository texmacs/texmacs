;;;; 	Copyright (C) 1997, 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 


(define-module (ice-9 session)
  :use-module (ice-9 documentation)
  :use-module (ice-9 regex)
  )



;;; Documentation
;;;
(define-public help
  (procedure->syntax
    (lambda (exp env)
      "(help [NAME])
Prints useful information.  Try `(help)'."
      (cond ((not (= (length exp) 2))
	     (help-usage))
	    ((not (feature? 'regex))
	     (display "`help' depends on the `regex' feature.
You don't seem to have regular expressions installed.\n"))
	    (else
	     (let ((name (cadr exp)))
	       (cond ((symbol? name)
		      (help-doc name
				(string-append "^"
					       (regexp-quote
						(symbol->string name))
					       "$")))
		     ((string? name)
		      (help-doc name name))
		     ((and (list? name)
			   (= (length name) 2)
			   (eq? (car name) 'unquote))
		      (let ((doc (object-documentation (local-eval (cadr name)
								   env))))
			(if (not doc)
			    (simple-format #t "No documentation found for ~S\n"
					   (cadr name))
			    (write-line doc))))
		     (else
		      (help-usage)))
	       *unspecified*))))))

(define (help-doc term regexp)
  (let ((entries (apropos-fold (lambda (module name object data)
				 (cons (list module
					     name
					     (object-documentation object))
				       data))
			       '()
			       regexp
			       apropos-fold-exported))
	(module car)
	(name cadr)
	(doc caddr))
    (if (null? entries)
	;; no matches
	(begin
	  (display "Did not find any object ")
	  (simple-format #t
			 (if (symbol? term)
			     "named `~A'\n"
			     "matching regexp \"~A\"\n")
			 term))
	(let ((first? #t))
	  (if (or-map doc entries)
	      ;; entries with documentation
	      (for-each (lambda (entry)
			  ;; *fixme*: Use `describe' when we have GOOPS?
			  (if (doc entry)
			      (begin
				(if first?
				    (set! first? #f)
				    (newline))
				(simple-format #t "~S: ~S\n~A\n"
					       (module-name (module entry))
					       (name entry)
					       (doc entry)))))
			entries))
	  (if (or-map (lambda (x) (not (doc x))) entries)
	      ;; entries without documentation
	      (begin
		(if (not first?)
		    (display "\nNo documentation found for:\n"))
		(for-each (lambda (entry)
			    (if (not (doc entry))
				(simple-format #t "~S: ~S\n"
					       (module-name (module entry))
					       (name entry))))
			  entries)))))))

(define (help-usage)
  (display "Usage: (help NAME) gives documentation about objects named NAME (a symbol)
       (help REGEXP) ditto for objects with names matching REGEXP (a string)
       (help ,EXPR) gives documentation for object returned by EXPR
       (help) gives this text

`help' searches among bindings exported from loaded modules, while
`apropos' searches among bindings visible from the \"current\" module.

Examples: (help help)
          (help cons)
          (help \"output-string\")

Other useful sources of helpful information:

(apropos STRING)
(arity PROCEDURE)
(name PROCEDURE-OR-MACRO)
(source PROCEDURE-OR-MACRO)

Tools:

(backtrace)				;show backtrace from last error
(debug)					;enter the debugger
(trace [PROCEDURE])			;trace procedure (no arg => show)
(untrace [PROCEDURE])			;untrace (no arg => untrace all)

(OPTIONSET-options 'full)		;display option information
(OPTIONSET-enable 'OPTION)
(OPTIONSET-disable 'OPTION)
(OPTIONSET-set! OPTION VALUE)

where OPTIONSET is one of debug, read, eval, print

"))

;;; {Apropos}
;;;
;;; Author: Roland Orre <orre@nada.kth.se>
;;;

(define (id x) x)

(define-public (apropos rgx . options)
  "Search for bindings: apropos regexp {options= 'full 'shadow 'value}"
  (if (zero? (string-length rgx))
      "Empty string not allowed"
      (let* ((match (make-regexp rgx))
	     (modules (cons (current-module)
			    (module-uses (current-module))))
	     (separator #\tab)
	     (shadow (member 'shadow options))
	     (value (member 'value options)))
	(cond ((member 'full options)
	       (set! shadow #t)
	       (set! value #t)))
	(for-each
	 (lambda (module)
	   (let* ((builtin (or (eq? module the-scm-module)
			       (eq? module the-root-module)))
		  (name (module-name module))
		  (obarrays (if builtin
				(list (builtin-weak-bindings)
				      (builtin-bindings))
				(list (module-obarray module))))
		  (get-refs (if builtin
				(list id id)
				(list variable-ref)))
		  )
	     (for-each
	      (lambda (obarray get-ref)
		(array-for-each
		 (lambda (oblist)
		   (for-each
		    (lambda (x)
		      (cond ((regexp-exec match (car x))
			     (display name)
			     (display ": ")
			     (display (car x))
			     (cond ((procedure? (get-ref (cdr x)))
				    (display separator)
				    (display (get-ref (cdr x))))
				   (value
				    (display separator)
				    (display (get-ref (cdr x)))))
			     (if (and shadow
				      (not (eq? (module-ref module
							    (car x))
						(module-ref (current-module)
							    (car x)))))
				 (display " shadowed"))
			     (newline)
			     )))
		    oblist))
		 obarray))
	      obarrays get-refs)))
	 modules))))

(define-public (apropos-internal rgx)
  "Return a list of accessible variable names."
  (apropos-fold (lambda (module name var data)
		  (cons name data))
		'()
		rgx
		(apropos-fold-accessible (current-module))))

(define-public (apropos-fold proc init rgx folder)
  "Folds PROCEDURE over bindings matching third arg REGEXP.

Result is

  (PROCEDURE MODULE1 NAME1 VALUE1
    (PROCEDURE MODULE2 NAME2 VALUE2
      ...
      (PROCEDURE MODULEn NAMEn VALUEn INIT)))

where INIT is the second arg to `apropos-fold'.

Fourth arg FOLDER is one of

  (apropos-fold-accessible MODULE) ;fold over bindings accessible in MODULE
  apropos-fold-exported		   ;fold over all exported bindings
  apropos-fold-all		   ;fold over all bindings"
  (let ((match (make-regexp rgx))
	(recorded (make-vector 61 '())))
    (let ((fold-module
	   (lambda (module data)
	     (let* ((obarray-filter
		     (lambda (name val data)
		       (if (and (regexp-exec match name)
				(not (hashq-get-handle recorded name)))
			   (begin
			     (hashq-set! recorded name #t)
			     (proc module name val data))
			   data)))
		    (module-filter
		     (lambda (name var data)
		       (obarray-filter name (variable-ref var) data))))
	       (cond ((or (eq? module the-scm-module)
			  (eq? module the-root-module))
		      (hash-fold obarray-filter
				 (hash-fold obarray-filter
					    data
					    (builtin-bindings))
				 (builtin-weak-bindings)))
		     (module (hash-fold module-filter
					data
					(module-obarray module)))
		     (else data))))))
      (folder fold-module init))))

(define (make-fold-modules init-thunk traverse extract)
  "Return procedure capable of traversing a forest of modules.
The forest traversed is the image of the forest generated by root
modules returned by INIT-THUNK and the generator TRAVERSE.
It is an image under the mapping EXTRACT."
  (lambda (fold-module init)
    (let* ((table (make-hash-table 31))
	   (first? (lambda (obj)
		     (let* ((handle (hash-create-handle! table obj #t))
			    (first? (cdr handle)))
		       (set-cdr! handle #f)
		       first?))))
      (let rec ((data init)
		(modules (init-thunk)))
	(do ((modules modules (cdr modules))
	     (data data (if (first? (car modules))
			    (rec (fold-module (extract (car modules)) data)
				 (traverse (car modules)))
			    data)))
	    ((null? modules) data))))))

(define-public (apropos-fold-accessible module)
  (make-fold-modules (lambda () (list module))
		     module-uses
		     (lambda (x) x)))

(define (root-modules)
  (cons the-root-module
	(submodules (nested-ref the-root-module '(app modules)))))

(define (submodules m)
  (hash-fold (lambda (name var data)
	       (let ((obj (variable-ref var)))
		 (if (and (module? obj)
			  (eq? (module-kind obj) 'directory))
		     (cons obj data)
		     data)))
	     '()
	     (module-obarray m)))

(define-public apropos-fold-exported
  (make-fold-modules root-modules submodules module-public-interface))

(define-public apropos-fold-all
  (make-fold-modules root-modules submodules (lambda (x) x)))

(define-public (source obj)
  (cond ((procedure? obj) (procedure-source obj))
	((macro? obj) (procedure-source (macro-transformer obj)))
	(else #f)))

(define-public (arity obj)
  (let ((arity (procedure-property obj 'arity)))
    (display (car arity))
    (cond ((caddr arity)
	   (display " or more"))
	  ((not (zero? (cadr arity)))
	   (display " required and ")
	   (display (cadr arity))
	   (display " optional")))
    (if (and (not (caddr arity))
	     (= (car arity) 1)
	     (<= (cadr arity) 1))
	(display " argument")
	(display " arguments"))
    (if (closure? obj)
	(let ((formals (cadr (procedure-source obj))))
	  (if (pair? formals)
	      (begin
		(display ": `")
		(display (car formals))
		(let loop ((ls (cdr formals)))
		  (cond ((null? ls)
			 (display #\'))
			((not (pair? ls))
			 (display "', the rest in `")
			 (display ls)
			 (display #\'))
			(else
			 (if (pair? (cdr ls))
			     (display "', `")
			     (display "' and `"))
			 (display (car ls))
			 (loop (cdr ls))))))
	      (begin
		(display " in `")
		(display formals)
		(display #\')))))
    (display ".\n")))

(define-public system-module
  (procedure->syntax
   (lambda (exp env)
     (let* ((m (nested-ref the-root-module
			   (append '(app modules) (cadr exp)))))
       (if (not m)
	   (error "Couldn't find any module named" (cadr exp)))
       (let ((s (not (procedure-property (module-eval-closure m)
					 'system-module))))
	 (set-system-module! m s)
	 (string-append "Module " (symbol->string (module-name m))
			" is now a " (if s "system" "user") " module."))))))
