;;; installed-scm-file

;;;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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


;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this 
;;; file.
;;;


;;; {Features}
;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

;;; Return #t iff FEATURE is available to this Guile interpreter.
;;; In SLIB, provided? also checks to see if the module is available.
;;; We should do that too, but don't.
(define (provided? feature)
  (and (memq feature *features*) #t))

;;; presumably deprecated.
(define feature? provided?)

;;; let format alias simple-format until the more complete version is loaded
(define format simple-format)


;;; {R4RS compliance}

(primitive-load-path "ice-9/r4rs.scm")


;;; {Simple Debugging Tools}
;;


;; peek takes any number of arguments, writes them to the
;; current ouput port, and returns the last argument.
;; It is handy to wrap around an expression to look at
;; a value each time is evaluated, e.g.:
;;
;;	(+ 10 (troublesome-fn))
;;	=> (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
;;

(define (peek . stuff)
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define pk peek)

(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (display stuff)
      (newline)
      (car (last-pair stuff)))))


;;; {Trivial Functions}
;;;

(define (id x) x)
(define (1+ n) (+ n 1))
(define (-1+ n) (+ n -1))
(define 1- -1+)
(define return-it noop)
(define (and=> value procedure) (and value (procedure value)))
(define (make-hash-table k) (make-vector k '()))

;;; apply-to-args is functionally redunant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to 
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;	(apply-to-args (return-3d-mouse-coords)
;;;	  (lambda (x y z) 
;;;		...))
;;;

(define (apply-to-args args fn) (apply fn args))


;;; {Integer Math}
;;;

(define (ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (ipow-by-squaring (proc x x)
				(quotient k 2)
				(if (even? k) acc (proc acc x))
				proc))))

(define string-character-length string-length)



;; A convenience function for combining flag bits.  Like logior, but
;; handles the cases of 0 and 1 arguments.
;;
(define (flags . args)
  (cond
   ((null? args) 0)
   ((null? (cdr args)) (car args))
   (else (apply logior args))))


;;; {Symbol Properties}
;;;

(define (symbol-property sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (and pair (cdr pair))))

(define (set-symbol-property! sym prop val)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(set-cdr! pair val)
	(symbol-pset! sym (acons prop val (symbol-pref sym))))))

(define (symbol-property-remove! sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(symbol-pset! sym (delq! pair (symbol-pref sym))))))



;;; {Line and Delimited I/O}

;;; corresponds to SCM_LINE_INCREMENTORS in libguile.
(define scm-line-incrementors "\n")

(define (read-line! string . maybe-port)
  (let* ((port (if (pair? maybe-port)
		   (car maybe-port)
		   (current-input-port))))
    (let* ((rv (%read-delimited! scm-line-incrementors
				 string
				 #t
				 port))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((and (= nchars 0)
		  (eof-object? terminator))
	     terminator)
	    ((not terminator) #f)
	    (else nchars)))))

(define (read-delimited! delims buf . args)
  (let* ((num-args (length args))
	 (port (if (> num-args 0)
		   (car args)
		   (current-input-port)))
	 (handle-delim (if (> num-args 1)
			   (cadr args)
			   'trim))
	 (start (if (> num-args 2)
		    (caddr args)
		    0))
	 (end (if (> num-args 3)
		  (cadddr args)
		  (string-length buf))))
    (let* ((rv (%read-delimited! delims
				 buf
				 (not (eq? handle-delim 'peek))
				 port
				 start
				 end))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((or (not terminator)	; buffer filled
		 (eof-object? terminator))
	     (if (zero? nchars)
		 (if (eq? handle-delim 'split)
		     (cons terminator terminator)
		     terminator)
		 (if (eq? handle-delim 'split)
		     (cons nchars terminator)
		     nchars)))
	    (else
	     (case handle-delim
	       ((trim peek) nchars)
	       ((concat) (string-set! buf (+ nchars start) terminator)
			 (+ nchars 1))
	       ((split) (cons nchars terminator))
	       (else (error "unexpected handle-delim value: " 
			    handle-delim))))))))
  
(define (read-delimited delims . args)
  (let* ((port (if (pair? args)
		   (let ((pt (car args)))
		     (set! args (cdr args))
		     pt)
		   (current-input-port)))
	 (handle-delim (if (pair? args)
			   (car args)
			   'trim)))
    (let loop ((substrings ())
	       (total-chars 0)
	       (buf-size 100))		; doubled each time through.
      (let* ((buf (make-string buf-size))
	     (rv (%read-delimited! delims
				   buf
				   (not (eq? handle-delim 'peek))
				   port))
	     (terminator (car rv))
	     (nchars (cdr rv))
	     (join-substrings
	      (lambda ()
		(apply string-append
		       (reverse
			(cons (if (and (eq? handle-delim 'concat)
				       (not (eof-object? terminator)))
				  (string terminator)
				  "")
			      (cons (make-shared-substring buf 0 nchars)
				    substrings))))))
	     (new-total (+ total-chars nchars)))
	(cond ((not terminator)
	       ;; buffer filled.
	       (loop (cons (substring buf 0 nchars) substrings)
		     new-total
		     (* buf-size 2)))
	      ((eof-object? terminator)
	       (if (zero? new-total)
		   (if (eq? handle-delim 'split)
		       (cons terminator terminator)
		       terminator)
		   (if (eq? handle-delim 'split)
		       (cons (join-substrings) terminator)
		       (join-substrings))))
	      (else
	       (case handle-delim
		   ((trim peek concat) (join-substrings))
		   ((split) (cons (join-substrings) terminator))


		   (else (error "unexpected handle-delim value: "
				handle-delim)))))))))

;;; read-line [PORT [HANDLE-DELIM]] reads a newline-terminated string
;;; from PORT.  The return value depends on the value of HANDLE-DELIM,
;;; which may be one of the symbols `trim', `concat', `peek' and
;;; `split'.  If it is `trim' (the default), the trailing newline is
;;; removed and the string is returned.  If `concat', the string is
;;; returned with the trailing newline intact.  If `peek', the newline
;;; is left in the input port buffer and the string is returned.  If
;;; `split', the newline is split from the string and read-line
;;; returns a pair consisting of the truncated string and the newline.

(define (read-line . args)
  (let* ((port		(if (null? args)
			    (current-input-port)
			    (car args)))
	 (handle-delim	(if (> (length args) 1)
			    (cadr args)
			    'trim))
	 (line/delim	(%read-line port))
	 (line		(car line/delim))
	 (delim		(cdr line/delim)))
    (case handle-delim
      ((trim) line)
      ((split) line/delim)
      ((concat) (if (and (string? line) (char? delim))
		    (string-append line (string delim))
		    line))
      ((peek) (if (char? delim)
		  (unread-char delim port))
	      line)
      (else
       (error "unexpected handle-delim value: " handle-delim)))))


;;; {Arrays}
;;;

(if (provided? 'array)
    (primitive-load-path "ice-9/arrays.scm"))


;;; {Keywords}
;;;

(define (symbol->keyword symbol)
  (make-keyword-from-dash-symbol (symbol-append '- symbol)))

(define (keyword->symbol kw)
  (let ((sym (keyword-dash-symbol kw)))
    (string->symbol (substring sym 1 (string-length sym)))))

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {Structs}

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))


;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

(define (inherit-print-state old-port new-port)
  (if (get-print-state old-port)
      (port-with-print-state new-port (get-print-state old-port))
      new-port))

;; 0: type-name, 1: fields
(define record-type-vtable 
  (make-vtable-vtable "prpr" 0
		      (lambda (s p)
			(cond ((eq? s record-type-vtable)
			       (display "#<record-type-vtable>" p))
			      (else
			       (display "#<record-type " p)
			       (display (record-type-name s) p)
			       (display ">" p))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let ((struct (make-struct record-type-vtable 0
			       (make-struct-layout
				(apply symbol-append
				       (map (lambda (f) "pw") fields)))
			       (or printer-fn
				   (lambda (s p)
				     (display "#<" p)
				     (display type-name p)
				     (let loop ((fields fields)
						(off 0))
				       (cond
					((not (null? fields))
					 (display " " p)
					 (display (car fields) p)
					 (display ": " p)
					 (display (struct-ref s off) p)
					 (loop (cdr fields) (+ 1 off)))))
				     (display ">" p)))
			       type-name
			       (copy-tree fields))))
      ;; Temporary solution: Associate a name to the record type descriptor
      ;; so that the object system can create a wrapper class for it.
      (set-struct-vtable-name! struct (if (symbol? type-name)
					  type-name
					  (string->symbol type-name)))
      struct)))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (record-type-fields rtd))))
    (eval `(lambda ,field-names
	     (make-struct ',rtd 0 ,@(map (lambda (f)
					  (if (memq f field-names)
					      f
					      #f))
					(record-type-fields rtd)))))))

(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (record-accessor rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-ref obj ,pos))))))

(define (record-modifier rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj val)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-set! obj ,pos val))))))


(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(provide 'record)


;;; {Booleans}
;;;

(define (->bool x) (not (not x)))


;;; {Symbols}
;;;

(define (symbol-append . args)
  (string->symbol (apply string-append args)))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))

(define (obarray-symbol-append ob . args)
  (string->obarray-symbol (apply string-append ob args)))

(define (obarray-gensym obarray . opt)
  (if (null? opt)
      (gensym "%%gensym" obarray)
      (gensym (car opt) obarray)))


;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eq? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

(define (make-list n . init)
  (if (pair? init) (set! init (car init)))
  (let loop ((answer '())
	     (n n))
    (if (<= n 0)
	answer
	(loop (cons init answer) (- n 1)))))



;;; {Multiple return values}

(define *values-rtd*
  (make-record-type "values"
		    '(values)))

(define values
  (let ((make-values (record-constructor *values-rtd*)))
    (lambda x
      (if (and (not (null? x))
	       (null? (cdr x)))
	  (car x)
	  (make-values x)))))

(define call-with-values
  (let ((access-values (record-accessor *values-rtd* 'values))
	(values-predicate? (record-predicate *values-rtd*)))
    (lambda (producer consumer)
      (let ((result (producer)))
	(if (values-predicate? result)
	    (apply consumer (access-values result))
	    (consumer result))))))

(provide 'values)


;;; {and-map and or-map}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (map-in-order fn lst) is like (map fn lst) but definately in order of lst.
;;;

;; and-map f l
;;
;; Apply f to successive elements of l until exhaustion or f returns #f.
;; If returning early, return #f.  Otherwise, return the last value returned
;; by f.  If f has never been called because l is empty, return #t.
;; 
(define (and-map f lst)
  (let loop ((result #t)
	     (l lst))
    (and result
	 (or (and (null? l)
		  result)
	     (loop (f (car l)) (cdr l))))))

;; or-map f l
;;
;; Apply f to successive elements of l until exhaustion or while f returns #f.
;; If returning early, return the return value of f.
;;
(define (or-map f lst)
  (let loop ((result #f)
	     (l lst))
    (or result
	(and (not (null? l))
	     (loop (f (car l)) (cdr l))))))



(if (provided? 'posix)
    (primitive-load-path "ice-9/posix.scm"))

(if (provided? 'socket)
    (primitive-load-path "ice-9/networking.scm"))

(define file-exists?
  (if (provided? 'posix)
      (lambda (str)
	(access? str F_OK))
      (lambda (str)
	(let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define file-is-directory?
  (if (provided? 'posix)
      (lambda (str)
	(eq? (stat:type (stat str)) 'directory))
      (lambda (str)
	(let ((port (catch 'system-error
			   (lambda () (open-file (string-append str "/.")
						 OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))


;;; {Error Handling}
;;;

(define (error . args)
  (save-stack)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "~A")
		 (rest (cdr args)))
	(if (not (null? rest))
	    (loop (string-append msg " ~S")
		  (cdr rest))
	    (scm-error 'misc-error #f msg args #f)))))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
	(apply error "unhandled-exception:" key args))))



(define (tm:sec obj) (vector-ref obj 0))
(define (tm:min obj) (vector-ref obj 1))
(define (tm:hour obj) (vector-ref obj 2))
(define (tm:mday obj) (vector-ref obj 3))
(define (tm:mon obj) (vector-ref obj 4))
(define (tm:year obj) (vector-ref obj 5))
(define (tm:wday obj) (vector-ref obj 6))
(define (tm:yday obj) (vector-ref obj 7))
(define (tm:isdst obj) (vector-ref obj 8))
(define (tm:gmtoff obj) (vector-ref obj 9))
(define (tm:zone obj) (vector-ref obj 10))

(define (set-tm:sec obj val) (vector-set! obj 0 val))
(define (set-tm:min obj val) (vector-set! obj 1 val))
(define (set-tm:hour obj val) (vector-set! obj 2 val))
(define (set-tm:mday obj val) (vector-set! obj 3 val))
(define (set-tm:mon obj val) (vector-set! obj 4 val))
(define (set-tm:year obj val) (vector-set! obj 5 val))
(define (set-tm:wday obj val) (vector-set! obj 6 val))
(define (set-tm:yday obj val) (vector-set! obj 7 val))
(define (set-tm:isdst obj val) (vector-set! obj 8 val))
(define (set-tm:gmtoff obj val) (vector-set! obj 9 val))
(define (set-tm:zone obj val) (vector-set! obj 10 val))

(define (tms:clock obj) (vector-ref obj 0))
(define (tms:utime obj) (vector-ref obj 1))
(define (tms:stime obj) (vector-ref obj 2))
(define (tms:cutime obj) (vector-ref obj 3))
(define (tms:cstime obj) (vector-ref obj 4))

(define (file-position . args) (apply ftell args))
(define (file-set-position . args) (apply fseek args))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
	 (dup->fdes fd/port fd)
	 (close fd/port)
	 fd)
	(else
	 (primitive-move->fdes fd/port fd)
	 (set-port-revealed! fd/port 1)
	 fd/port)))

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
	(set-port-revealed! port (- revealed 1)))))

(define (dup->port port/fd mode . maybe-fd)
  (let ((port (fdopen (apply dup->fdes port/fd maybe-fd)
		      mode)))
    (if (pair? maybe-fd)
	(set-port-revealed! port 1))
    port))
  
(define (dup->inport port/fd . maybe-fd)
  (apply dup->port port/fd "r" maybe-fd))

(define (dup->outport port/fd . maybe-fd)
  (apply dup->port port/fd "w" maybe-fd))

(define (dup port/fd . maybe-fd)
  (if (integer? port/fd)
      (apply dup->fdes port/fd maybe-fd)
      (apply dup->port port/fd (port-mode port/fd) maybe-fd)))

(define (duplicate-port port modes)
  (dup->port port modes))

(define (fdes->inport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "r")))
	     (set-port-revealed! result 1)
	     result))
	  ((input-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (fdes->outport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "w")))
	     (set-port-revealed! result 1)
	     result))
	  ((output-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (port->fdes port)
  (set-port-revealed! port (+ (port-revealed port) 1))
  (fileno port))

(define (setenv name value)
  (if value
      (putenv (string-append name "=" value))
      (putenv name)))


;;; {Load Paths}
;;;

;;; Here for backward compatability
;;
(define scheme-file-suffix (lambda () ".scm"))

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
		(if (zero? len)
		    #f
		    (string-ref vicinity (- len 1))))))
    (string-append vicinity
		   (if (or (not tail)
			   (eq? tail #\/))
		       ""
		       "/")
		   file)))


;;; {Help for scm_shell}
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.
(define (load-user-init)
  (let* ((home (or (getenv "HOME")
		   (false-if-exception (passwd:dir (getpwuid (getuid))))
		   "/"))  ;; fallback for cygwin etc.
	 (init-file (in-vicinity home ".guile")))
    (if (file-exists? init-file)
	(primitive-load init-file))))


;;; {Loading by paths}

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
	       (primitive-load-path name)))



;;; {Transcendental Functions}
;;;
;;; Derived from "Transcen.scm", Complex trancendental functions for SCM.
;;; Written by Jerry D. Hedden, (C) FSF.
;;; See the file `COPYING' for terms applying to this program.
;;;

(define (exp z)
  (if (real? z) ($exp z)
      (make-polar ($exp (real-part z)) (imag-part z))))

(define (log z)
  (if (and (real? z) (>= z 0))
      ($log z)
      (make-rectangular ($log (magnitude z)) (angle z))))

(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 ($sqrt (- z)))
	  ($sqrt z))
      (make-polar ($sqrt (magnitude z)) (/ (angle z) 2))))

(define expt
  (let ((integer-expt integer-expt))
    (lambda (z1 z2)
      (cond ((exact? z2)
	     (integer-expt z1 z2))
	    ((and (real? z2) (real? z1) (>= z1 0))
	     ($expt z1 z2))
	    (else
	     (exp (* z2 (log z1))))))))

(define (sinh z)
  (if (real? z) ($sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sinh x) ($cos y))
			  (* ($cosh x) ($sin y))))))
(define (cosh z)
  (if (real? z) ($cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cosh x) ($cos y))
			  (* ($sinh x) ($sin y))))))
(define (tanh z)
  (if (real? z) ($tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cosh x) ($cos y))))
	(make-rectangular (/ ($sinh x) w) (/ ($sin y) w)))))

(define (asinh z)
  (if (real? z) ($asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (acosh z)
  (if (and (real? z) (>= z 1))
      ($acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      ($atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

(define (sin z)
  (if (real? z) ($sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sin x) ($cosh y))
			  (* ($cos x) ($sinh y))))))
(define (cos z)
  (if (real? z) ($cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cos x) ($cosh y))
			  (- (* ($sin x) ($sinh y)))))))
(define (tan z)
  (if (real? z) ($tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cos x) ($cosh y))))
	(make-rectangular (/ ($sin x) w) (/ ($sinh y) w)))))

(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($asin z)
      (* -i (asinh (* +i z)))))

(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

(define (atan z . y)
  (if (null? y)
      (if (real? z) ($atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))

(define (log10 arg)
  (/ (log arg) (log 10)))



;;; {Reader Extensions}
;;;

;;; Reader code for various "#c" forms.
;;;

(read-hash-extend #\' (lambda (c port)
			(read port)))
(read-hash-extend #\. (lambda (c port)
			(eval (read port))))


;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
	(eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
			   (string-length (car argv))))
	   (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
	   (kw-opt? (member kw kw-opts))
	   (kw-arg? (member kw kw-args))
	   (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
			 (substring (car argv)
				    (+ kw-arg-pos 1)
				    (string-length (car argv))))
		    (and kw-arg?
			 (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
	  (return kw arg (cdr argv))
	  (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
	   (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cdr argv)
			     (cons (string-append "-" rest-car) (cdr argv)))))
	  (return kw #f new-argv)))

       ((member kw kw-args)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (arg (if (= 0 (string-length rest-car))
			(cadr argv)
			rest-car))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cddr argv)
			     (cdr argv))))
	  (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
		(lambda (opt opt-arg argv)
		  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
	 (eq? (car kw) 'else)
	 (let* ((opt-desc kw)
		(help (cadr opt-desc))
		(opts (car opt-desc))
		(opts-proper (if (string? (car opts)) (cdr opts) opts))
		(arg-name (if (string? (car opts))
			      (string-append "<" (car opts) ">")
			      ""))
		(left-part (string-append
			    (with-output-to-string
			      (lambda ()
				(map (lambda (x) (display (keyword-symbol x)) (display " "))
				     opts-proper)))
			    arg-name))
		(middle-part (if (and (< (string-length left-part) 30)
				      (< (string-length help) 40))
				 (make-string (- 30 (string-length left-part)) #\ )
				 "\n\t")))
	   (display left-part)
	   (display middle-part)
	   (display help)
	   (newline))))
   kw-desc))
		  

	   
(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
	 (usage-sans-specials (map (lambda (x)
				    (or (and (not (list? x)) x)
					(and (symbol? (car x)) #t)
					(and (boolean? (car x)) #t)
					x))
				  raw-usage))
	 (usage-desc (delq! #t usage-sans-specials))
	 (kw-desc (map car usage-desc))
	 (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
	 (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
	 (transmogrified-cases (map (lambda (case)
				      (cons (let ((opts (car case)))
					      (if (or (boolean? opts) (eq? 'else opts))
						  opts
						  (cond
						   ((symbol? (car opts))  opts)
						   ((boolean? (car opts)) opts)
						   ((string? (caar opts)) (cdar opts))
						   (else (car opts)))))
					    (cdr case)))
				    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
	 (let %next-arg ((%argv %argv))
	   (get-option %argv
		       ',kw-opts
		       ',kw-args
		       (lambda (%opt %arg %new-argv)
			 (case %opt
			   ,@ transmogrified-cases))))))))




;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; !!! warning: The interface to lazy binder procedures is going
;;; to be changed in an incompatible way to permit all the basic
;;; module ops to be virtualized.
;;;
;;; (make-module size use-list lazy-binding-proc) => module
;;; module-{obarray,uses,binder}[|-set!]
;;; (module? obj) => [#t|#f]
;;; (module-locally-bound? module symbol) => [#t|#f]
;;; (module-bound? module symbol) => [#t|#f]
;;; (module-symbol-locally-interned? module symbol) => [#t|#f]
;;; (module-symbol-interned? module symbol) => [#t|#f]
;;; (module-local-variable module symbol) => [#<variable ...> | #f]
;;; (module-variable module symbol) => [#<variable ...> | #f]
;;; (module-symbol-binding module symbol opt-value)
;;;		=> [ <obj> | opt-value | an error occurs ]
;;; (module-make-local-var! module symbol) => #<variable...>
;;; (module-add! module symbol var) => unspecified
;;; (module-remove! module symbol) =>  unspecified
;;; (module-for-each proc module) => unspecified
;;; (make-scm-module) => module ; a lazy copy of the symhash module
;;; (set-current-module module) => unspecified
;;; (current-module) => #<module...>
;;;
;;;


;;; {Printing Modules}
;; This is how modules are printed.  You can re-define it.
;; (Redefining is actually more complicated than simply redefining
;; %print-module because that would only change the binding and not
;; the value stored in the vtable that determines how record are
;; printed. Sigh.)

(define (%print-module mod port)  ; unused args: depth length style table)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (let ((name (module-name mod)))
    (if name
	(begin
	  (display " " port)
	  (display name port))))
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

;; module-type
;;
;; A module is characterized by an obarray in which local symbols
;; are interned, a list of modules, "uses", from which non-local
;; bindings can be inherited, and an optional lazy-binder which
;; is a (CLOSURE module symbol) which, as a last resort, can provide
;; bindings that would otherwise not be found locally in the module.
;;
(define module-type
  (make-record-type 'module
		    '(obarray uses binder eval-closure transformer name kind
			      observers weak-observers observer-id)
		    %print-module))

;; make-module &opt size uses binder
;;
;; Create a new module, perhaps with a particular size of obarray,
;; initial uses list, or binding procedure.
;;
(define make-module
    (lambda args

      (define (parse-arg index default)
	(if (> (length args) index)
	    (list-ref args index)
	    default))

      (if (> (length args) 3)
	  (error "Too many args to make-module." args))

      (let ((size (parse-arg 0 1021))
	    (uses (parse-arg 1 '()))
	    (binder (parse-arg 2 #f)))

	(if (not (integer? size))
	    (error "Illegal size to make-module." size))
	(if (not (and (list? uses)
		      (and-map module? uses)))
	    (error "Incorrect use list." uses))
	(if (and binder (not (procedure? binder)))
	    (error
	     "Lazy-binder expected to be a procedure or #f." binder))

	(let ((module (module-constructor (make-vector size '())
					  uses binder #f #f #f #f
					  '()
					  (make-weak-value-hash-table 31)
					  0)))

	  ;; We can't pass this as an argument to module-constructor,
	  ;; because we need it to close over a pointer to the module
	  ;; itself.
	  (set-module-eval-closure! module (standard-eval-closure module))

	  module))))

(define module-constructor (record-constructor module-type))
(define module-obarray  (record-accessor module-type 'obarray))
(define set-module-obarray! (record-modifier module-type 'obarray))
(define module-uses  (record-accessor module-type 'uses))
(define set-module-uses! (record-modifier module-type 'uses))
(define module-binder (record-accessor module-type 'binder))
(define set-module-binder! (record-modifier module-type 'binder))

;; NOTE: This binding is used in libguile/modules.c.
(define module-eval-closure (record-accessor module-type 'eval-closure))

(define module-transformer (record-accessor module-type 'transformer))
(define set-module-transformer! (record-modifier module-type 'transformer))
(define module-name (record-accessor module-type 'name))
(define set-module-name! (record-modifier module-type 'name))
(define module-kind (record-accessor module-type 'kind))
(define set-module-kind! (record-modifier module-type 'kind))
(define module-observers (record-accessor module-type 'observers))
(define set-module-observers! (record-modifier module-type 'observers))
(define module-weak-observers (record-accessor module-type 'weak-observers))
(define module-observer-id (record-accessor module-type 'observer-id))
(define set-module-observer-id! (record-modifier module-type 'observer-id))
(define module? (record-predicate module-type))

(define set-module-eval-closure!
  (let ((setter (record-modifier module-type 'eval-closure)))
    (lambda (module closure)
      (setter module closure)
      ;; Make it possible to lookup the module from the environment.
      ;; This implementation is correct since an eval closure can belong
      ;; to maximally one module.
      (set-procedure-property! closure 'module module))))

(define (eval-in-module exp module)
  (eval2 exp (module-eval-closure module)))


;;; {Observer protocol}
;;;

(define (module-observe module proc)
  (set-module-observers! module (cons proc (module-observers module)))
  (cons module proc))

(define (module-observe-weak module proc)
  (let ((id (module-observer-id module)))
    (hash-set! (module-weak-observers module) id proc)
    (set-module-observer-id! module (+ 1 id))
    (cons module id)))

(define (module-unobserve token)
  (let ((module (car token))
	(id (cdr token)))
    (if (integer? id)
	(hash-remove! (module-weak-observers module) id)
	(set-module-observers! module (delq1! id (module-observers module)))))
  *unspecified*)

(define (module-modified m)
  (for-each (lambda (proc) (proc m)) (module-observers m))
  (hash-fold (lambda (id proc res) (proc m)) #f (module-weak-observers m)))


;;; {Module Searching in General}
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the obarray of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; DISPLAY is locally rebound in the module `safe-guile'.''
;;;
;;;
;;; Other times, we want to test for a symbol property in the obarray
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: ``The symbol 'fnord is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M.''
;;;

;; module-search fn m
;; 
;; return the first non-#f result of FN applied to M and then to
;; the modules in the uses of m, and so on recursively.  If all applications
;; return #f, then so does this function.
;;
(define (module-search fn m v)
  (define (loop pos)
    (and (pair? pos)
	 (or (module-search fn (car pos) v)
	     (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;; {Is a symbol bound in a module?}
;;;
;;; Symbol S in Module M is bound if S is interned in M and if the binding
;;; of S in M has been set to some well-defined value.
;;;

;; module-locally-bound? module symbol
;;
;; Is a symbol bound (interned and defined) locally in a given module?
;;
(define (module-locally-bound? m v)
  (let ((var (module-local-variable m v)))
    (and var
	 (variable-bound? var))))

;; module-bound? module symbol
;;
;; Is a symbol bound (interned and defined) anywhere in a given module
;; or its uses?
;;
(define (module-bound? m v)
  (module-search module-locally-bound? m v))

;;; {Is a symbol interned in a module?}
;;;
;;; Symbol S in Module M is interned if S occurs in 
;;; of S in M has been set to some well-defined value.
;;;
;;; It is possible to intern a symbol in a module without providing
;;; an initial binding for the corresponding variable.  This is done
;;; with:
;;;       (module-add! module symbol (make-undefined-variable))
;;;
;;; In that case, the symbol is interned in the module, but not
;;; bound there.  The unbound symbol shadows any binding for that
;;; symbol that might otherwise be inherited from a member of the uses list.
;;;

(define (module-obarray-get-handle ob key)
  ((if (symbol? key) hashq-get-handle hash-get-handle) ob key))

(define (module-obarray-ref ob key)
  ((if (symbol? key) hashq-ref hash-ref) ob key))

(define (module-obarray-set! ob key val)
  ((if (symbol? key) hashq-set! hash-set!) ob key val))

(define (module-obarray-remove! ob key)
  ((if (symbol? key) hashq-remove! hash-remove!) ob key))

;; module-symbol-locally-interned? module symbol
;; 
;; is a symbol interned (not neccessarily defined) locally in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-locally-interned? m v)
  (not (not (module-obarray-get-handle (module-obarray m) v))))

;; module-symbol-interned? module symbol
;; 
;; is a symbol interned (not neccessarily defined) anywhere in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-interned? m v)
  (module-search module-symbol-locally-interned? m v))


;;; {Mapping modules x symbols --> variables}
;;;

;; module-local-variable module symbol
;; return the local variable associated with a MODULE and SYMBOL.
;;
;;; This function is very important. It is the only function that can
;;; return a variable from a module other than the mutators that store
;;; new variables in modules.  Therefore, this function is the location
;;; of the "lazy binder" hack.
;;;
;;; If symbol is defined in MODULE, and if the definition binds symbol
;;; to a variable, return that variable object.
;;;
;;; If the symbols is not found at first, but the module has a lazy binder,
;;; then try the binder.
;;;
;;; If the symbol is not found at all, return #f.
;;;
(define (module-local-variable m v)
;  (caddr
;   (list m v
	 (let ((b (module-obarray-ref (module-obarray m) v)))
	   (or (and (variable? b) b)
	       (and (module-binder m)
		    ((module-binder m) m v #f)))))
;))

;; module-variable module symbol
;; 
;; like module-local-variable, except search the uses in the 
;; case V is not found in M.
;;
;; NOTE: This function is superseded with C code (see modules.c)
;;;      when using the standard eval closure.
;;
(define (module-variable m v)
  (module-search module-local-variable m v))


;;; {Mapping modules x symbols --> bindings}
;;;
;;; These are similar to the mapping to variables, except that the
;;; variable is dereferenced.
;;;

;; module-symbol-binding module symbol opt-value
;; 
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-local-binding m v . opt-val)
  (let ((var (module-local-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Locally unbound variable." v)))))

;; module-symbol-binding module symbol opt-value
;; 
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-binding m v . opt-val)
  (let ((var (module-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Unbound variable." v)))))



;;; {Adding Variables to Modules}
;;;
;;;


;; module-make-local-var! module symbol
;; 
;; ensure a variable for V in the local namespace of M.
;; If no variable was already there, then create a new and uninitialzied
;; variable.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
	(and (variable? b)
	     (begin
	       (module-modified m)
	       b)))
      (and (module-binder m)
	   ((module-binder m) m v #t))
      (begin
	(let ((answer (make-undefined-variable v)))
	  (module-obarray-set! (module-obarray m) v answer)
	  (module-modified m)
	  answer))))

;; module-add! module symbol var
;; 
;; ensure a particular variable for V in the local namespace of M.
;;
(define (module-add! m v var)
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (module-obarray-set! (module-obarray m) v var)
  (module-modified m))

;; module-remove! 
;; 
;; make sure that a symbol is undefined in the local namespace of M.
;;
(define (module-remove! m v)
  (module-obarray-remove!  (module-obarray m) v)
  (module-modified m))

(define (module-clear! m)
  (vector-fill! (module-obarray m) '())
  (module-modified m))

;; MODULE-FOR-EACH -- exported
;; 
;; Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE).
;;
(define (module-for-each proc module)
  (let ((obarray (module-obarray module)))
    (do ((index 0 (+ index 1))
	 (end (vector-length obarray)))
	((= index end))
      (for-each
       (lambda (bucket)
	 (proc (car bucket) (cdr bucket)))
       (vector-ref obarray index)))))


(define (module-map proc module)
  (let* ((obarray (module-obarray module))
	 (end (vector-length obarray)))

    (let loop ((i 0)
	       (answer '()))
      (if (= i end)
	  answer
	  (loop (+ 1 i)
		(append!
		 (map (lambda (bucket)
			(proc (car bucket) (cdr bucket)))
		      (vector-ref obarray i))
		 answer))))))


;;; {Low Level Bootstrapping}
;;;

;; make-root-module 

;; A root module uses the symhash table (the system's privileged 
;; obarray).  Being inside a root module is like using SCM without
;; any module system.
;;


(define (root-module-closure m s define?)
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (or define? (variable-bound? bi))
	 (begin
	   (module-add! m s bi)
	   bi))))

(define (make-root-module)
  (make-module 1019 '() root-module-closure))


;; make-scm-module 

;; An scm module is a module into which the lazy binder copies
;; variable bindings from the system symhash table.  The mapping is
;; one way only; newly introduced bindings in an scm module are not
;; copied back into the system symhash table (and can be used to override
;; bindings from the symhash table).
;;

(define (scm-module-closure m s define?)
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (variable-bound? bi)
	 (begin
	   (module-add! m s bi)
	   bi))))

(define (make-scm-module)
  (make-module 1019 '() scm-module-closure))



;; the-module
;;
;; NOTE: This binding is used in libguile/modules.c.
;;
(define the-module #f)

;; scm:eval-transformer
;;
(define scm:eval-transformer #f)

;; set-current-module module
;;
;; set the current module as viewed by the normalizer.
;;
;; NOTE: This binding is used in libguile/modules.c.
;;
(define (set-current-module m)
  (set! the-module m)
  (if m
      (begin
	(set! *top-level-lookup-closure* (module-eval-closure the-module))
	(set! scm:eval-transformer (module-transformer the-module)))
      (set! *top-level-lookup-closure* #f)))


;; current-module
;;
;; return the current module as viewed by the normalizer.
;;
(define (current-module) the-module)

;;; {Module-based Loading}
;;;

(define (save-module-excursion thunk)
  (let ((inner-module (current-module))
	(outer-module #f))
    (dynamic-wind (lambda ()
		    (set! outer-module (current-module))
		    (set-current-module inner-module)
		    (set! inner-module #f))
		  thunk
		  (lambda ()
		    (set! inner-module (current-module))
		    (set-current-module outer-module)
		    (set! outer-module #f)))))

(define basic-load load)

(define (load-module filename)
  (save-module-excursion
   (lambda ()
     (let ((oldname (and (current-load-port)
			 (port-filename (current-load-port)))))
       (basic-load (if (and oldname
			    (> (string-length filename) 0)
			    (not (char=? (string-ref filename 0) #\/))
			    (not (string=? (dirname oldname) ".")))
		       (string-append (dirname oldname) "/" filename)
		       filename))))))



;;; {MODULE-REF -- exported}
;;
;; Returns the value of a variable called NAME in MODULE or any of its
;; used modules.  If there is no such variable, then if the optional third
;; argument DEFAULT is present, it is returned; otherwise an error is signaled.
;; 
(define (module-ref module name . rest)
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
	(variable-ref variable)
	(if (null? rest)
	    (error "No variable named" name 'in module)
	    (car rest)			; default value
	    ))))

;; MODULE-SET! -- exported
;;
;; Sets the variable called NAME in MODULE (or in a module that MODULE uses)
;; to VALUE; if there is no such variable, an error is signaled.
;; 
(define (module-set! module name value)
  (let ((variable (module-variable module name)))
    (if variable
	(variable-set! variable value)
	(error "No variable named" name 'in module))))

;; MODULE-DEFINE! -- exported
;;
;; Sets the variable called NAME in MODULE to VALUE; if there is no such
;; variable, it is added first.
;; 
(define (module-define! module name value)
  (let ((variable (module-local-variable module name)))
    (if variable
	(begin
	  (variable-set! variable value)
	  (module-modified module))
	(module-add! module name (make-variable value name)))))

;; MODULE-DEFINED? -- exported
;;
;; Return #t iff NAME is defined in MODULE (or in a module that MODULE
;; uses)
;;
(define (module-defined? module name)
  (let ((variable (module-variable module name)))
    (and variable (variable-bound? variable))))

;; MODULE-USE! module interface
;;
;; Add INTERFACE to the list of interfaces used by MODULE.
;; 
(define (module-use! module interface)
  (set-module-uses! module
		    (cons interface (delq! interface (module-uses module))))
  (module-modified module))


;;; {Recursive Namespaces}
;;;
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and variables bound to modules as nested namespaces.
;;;
;;; The routines in this file manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;		(nested-ref some-root-module '(foo bar baz))
;;;		=> <value of a variable named baz in the module bound to bar in 
;;;		    the module bound to foo in some-root-module>
;;;
;;;
;;; There are:
;;;
;;;	;; a-root is a module
;;;	;; name is a list of symbols
;;;
;;;	nested-ref a-root name
;;;	nested-set! a-root name val
;;;	nested-define! a-root name val
;;;	nested-remove! a-root name
;;;
;;;
;;; (current-module) is a natural choice for a-root so for convenience there are
;;; also:
;;;
;;;	local-ref name		==	nested-ref (current-module) name
;;;	local-set! name val	==	nested-set! (current-module) name val
;;;	local-define! name val	==	nested-define! (current-module) name val
;;;	local-remove! name	==	nested-remove! (current-module) name
;;;


(define (nested-ref root names)
  (let loop ((cur root)
	     (elts names))
    (cond
     ((null? elts)		cur)
     ((not (module? cur))	#f)
     (else (loop (module-ref cur (car elts) #f) (cdr elts))))))

(define (nested-set! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-set! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-define! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-define! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-remove! root names)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-remove! cur (car elts))
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (local-ref names) (nested-ref (current-module) names))
(define (local-set! names val) (nested-set! (current-module) names val))
(define (local-define names val) (nested-define! (current-module) names val))
(define (local-remove names) (nested-remove! (current-module) names))



;;; {The (app) module}
;;;
;;; The root of conventionally named objects not directly in the top level.
;;;
;;; (app modules)
;;; (app modules guile)
;;;
;;; The directory of all modules and the standard root module.
;;;

(define (module-public-interface m)
  (module-ref m '%module-public-interface #f))
(define (set-module-public-interface! m i)
  (module-define! m '%module-public-interface i))
(define (set-system-module! m s)
  (set-procedure-property! (module-eval-closure m) 'system-module s))
(define the-root-module (make-root-module))
(define the-scm-module (make-scm-module))
(set-module-public-interface! the-root-module the-scm-module)
(set-module-name! the-root-module '(guile))
(set-module-name! the-scm-module '(guile))
(set-module-kind! the-scm-module 'interface)
(for-each set-system-module! (list the-root-module the-scm-module) '(#t #t))

(set-current-module the-root-module)

(define app (make-module 31))
(local-define '(app modules) (make-module 31))
(local-define '(app modules guile) the-root-module)

;; (define-special-value '(app modules new-ws) (lambda () (make-scm-module)))

(define (try-load-module name)
  (or (try-module-linked name)
      (try-module-autoload name)
      (try-module-dynamic-link name)))

;; NOTE: This binding is used in libguile/modules.c.
;;
(define (resolve-module name . maybe-autoload)
  (let ((full-name (append '(app modules) name)))
    (let ((already (local-ref full-name)))
      (if already
	  ;; The module already exists...
	  (if (and (or (null? maybe-autoload) (car maybe-autoload))
		   (not (module-ref already '%module-public-interface #f)))
	      ;; ...but we are told to load and it doesn't contain source, so
	      (begin
		(try-load-module name)
		already)
	      ;; simply return it.
	      already)
	  (begin
	    ;; Try to autoload it if we are told so
	    (if (or (null? maybe-autoload) (car maybe-autoload))
		(try-load-module name))
	    ;; Get/create it.
	    (make-modules-in (current-module) full-name))))))
	    
(define (beautify-user-module! module)
  (let ((interface (module-public-interface module)))
    (if (or (not interface)
	    (eq? interface module))
	(let ((interface (make-module 31)))
	  (set-module-name! interface (module-name module))
	  (set-module-kind! interface 'interface)
	  (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
	   (not (eq? module the-root-module)))
      (set-module-uses! module (append (module-uses module) (list the-scm-module)))))

;; NOTE: This binding is used in libguile/modules.c.
;;
(define (make-modules-in module name)
  (if (null? name)
      module
      (cond
       ((module-ref module (car name) #f)
	=> (lambda (m) (make-modules-in m (cdr name))))
       (else	(let ((m (make-module 31)))
		  (set-module-kind! m 'directory)
		  (set-module-name! m (append (or (module-name module)
						  '())
					      (list (car name))))
		  (module-define! module (car name) m)
		  (make-modules-in m (cdr name)))))))

(define (resolve-interface name)
  (let ((module (resolve-module name)))
    (and module (module-public-interface module))))


(define %autoloader-developer-mode #t)

(define (process-define-module args)
  (let*  ((module-id (car args))
	  (module (resolve-module module-id #f))
	  (kws (cdr args)))
    (beautify-user-module! module)
    (let loop ((kws kws)
	       (reversed-interfaces '()))
      (if (null? kws)
	  (for-each (lambda (interface)
		      (module-use! module interface))
		    reversed-interfaces)
	  (let ((keyword (cond ((keyword? (car kws))
				(keyword->symbol (car kws)))
			       ((and (symbol? (car kws))
				     (eq? (string-ref (car kws) 0) #\:))
				(string->symbol (substring (car kws) 1)))
			       (else #f))))
	    (case keyword
	      ((use-module use-syntax)
	       (if (not (pair? (cdr kws)))
		   (error "unrecognized defmodule argument" kws))
	       (let* ((used-name (cadr kws))
		      (used-module (resolve-module used-name)))
		 (if (not (module-ref used-module
				      '%module-public-interface
				      #f))
		     (begin
		       ((if %autoloader-developer-mode warn error)
			"no code for module" (module-name used-module))
		       (beautify-user-module! used-module)))
		 (let ((interface (module-public-interface used-module)))
		   (if (not interface)
		       (error "missing interface for use-module"
			      used-module))
		   (if (eq? keyword 'use-syntax)
		       (set-module-transformer!
			module
			(module-ref interface (car (last-pair used-name))
				    #f)))
		   (loop (cddr kws)
			 (cons interface reversed-interfaces)))))
	      ((autoload)
	       (if (not (and (pair? (cdr kws)) (pair? (cddr kws))))
		   (error "unrecognized defmodule argument" kws))
	       (loop (cdddr kws)
		     (cons (make-autoload-interface module
						    (cadr kws)
						    (caddr kws))
			   reversed-interfaces)))
	      ((no-backtrace)
	       (set-system-module! module #t)
	       (loop (cdr kws) reversed-interfaces))
	      (else	
	       (error "unrecognized defmodule argument" kws))))))
    module))

;;; {Autoload}

(define (make-autoload-interface module name bindings)
  (let ((b (lambda (a sym definep)
	     (and (memq sym bindings)
		  (let ((i (module-public-interface (resolve-module name))))
		    (if (not i)
			(error "missing interface for module" name))
		    ;; Replace autoload-interface with interface
		    (set-car! (memq a (module-uses module)) i)
		    (module-local-variable i sym))))))
    (module-constructor #() '() b #f #f name 'autoload
			'() (make-weak-value-hash-table 31) 0)))


;;; {Autoloading modules}

(define autoloads-in-progress '())

(define (try-module-autoload module-name)
  (let* ((reverse-name (reverse module-name))
	 (name (car reverse-name))
	 (dir-hint-module-name (reverse (cdr reverse-name)))
	 (dir-hint (apply symbol-append (map (lambda (elt) (symbol-append elt "/")) dir-hint-module-name))))
    (resolve-module dir-hint-module-name #f)
    (and (not (autoload-done-or-in-progress? dir-hint name))
	 (let ((didit #f))
	   (dynamic-wind
	    (lambda () (autoload-in-progress! dir-hint name))
	    (lambda ()
	      (let ((full (%search-load-path (in-vicinity dir-hint name))))
		(if full
		    (begin
		      (save-module-excursion (lambda () (primitive-load full)))
		      (set! didit #t)))))
	    (lambda () (set-autoloaded! dir-hint name didit)))
	   didit))))


;;; Dynamic linking of modules

;; Initializing a module that is written in C is a two step process.
;; First the module's `module init' function is called.  This function
;; is expected to call `scm_register_module_xxx' to register the `real
;; init' function.  Later, when the module is referenced for the first
;; time, this real init function is called in the right context.  See
;; gtcltk-lib/gtcltk-module.c for an example.
;;
;; The code for the module can be in a regular shared library (so that
;; the `module init' function will be called when libguile is
;; initialized).  Or it can be dynamically linked.
;;
;; You can safely call `scm_register_module_xxx' before libguile
;; itself is initialized.  You could call it from an C++ constructor
;; of a static object, for example.
;;
;; To make your Guile extension into a dynamic linkable module, follow
;; these easy steps:
;;
;; - Find a name for your module, like (ice-9 gtcltk)
;; - Write a function with a name like
;;
;;     scm_init_ice_9_gtcltk_module
;;
;;   This is your `module init' function.  It should call
;;   
;;     scm_register_module_xxx ("ice-9 gtcltk", scm_init_gtcltk);
;;   
;;   "ice-9 gtcltk" is the C version of the module name. Slashes are
;;   replaced by spaces, the rest is untouched. `scm_init_gtcltk' is
;;   the real init function that executes the usual initializations
;;   like making new smobs, etc.
;;
;; - Make a shared library with your code and a name like
;;
;;     ice-9/libgtcltk.so
;;
;;   and put it somewhere in %load-path.
;;
;; - Then you can simply write `:use-module (ice-9 gtcltk)' and it
;;   will be linked automatically.
;;
;; This is all very experimental.

(define (split-c-module-name str)
  (let loop ((rev '())
	     (start 0)
	     (pos 0)
	     (end (string-length str)))
    (cond
     ((= pos end)
      (reverse (cons (string->symbol (substring str start pos)) rev)))
     ((eq? (string-ref str pos) #\space)
      (loop (cons (string->symbol (substring str start pos)) rev)
	    (+ pos 1)
	    (+ pos 1)
	    end))
     (else
      (loop rev start (+ pos 1) end)))))

(define (convert-c-registered-modules dynobj)
  (let ((res (map (lambda (c)
		    (list (split-c-module-name (car c)) (cdr c) dynobj))
		  (c-registered-modules))))
    (c-clear-registered-modules)
    res))

(define registered-modules '())

(define (register-modules dynobj)
  (set! registered-modules
	(append! (convert-c-registered-modules dynobj)
		 registered-modules)))

(define (init-dynamic-module modname)
  ;; Register any linked modules which has been registered on the C level
  (register-modules #f)
  (or-map (lambda (modinfo)
	    (if (equal? (car modinfo) modname)
		(begin
		  (set! registered-modules (delq! modinfo registered-modules))
		  (let ((mod (resolve-module modname #f)))
		    (save-module-excursion
		     (lambda ()
		       (set-current-module mod)
		       (set-module-public-interface! mod mod)
		       (dynamic-call (cadr modinfo) (caddr modinfo))
		       ))
		    #t))
		#f))
	  registered-modules))

(define (dynamic-maybe-call name dynobj)
  (catch #t				; could use false-if-exception here
	 (lambda ()
	   (dynamic-call name dynobj))
	 (lambda args
	   #f)))

(define (dynamic-maybe-link filename)
  (catch #t				; could use false-if-exception here
	 (lambda ()
	   (dynamic-link filename))
	 (lambda args
	   #f)))

(define (find-and-link-dynamic-module module-name)
  (define (make-init-name mod-name)
    (string-append "scm_init"
		   (list->string (map (lambda (c)
					(if (or (char-alphabetic? c)
						(char-numeric? c))
					    c
					    #\_))
				      (string->list mod-name)))
		   "_module"))

  ;; Put the subdirectory for this module in the car of SUBDIR-AND-LIBNAME,
  ;; and the `libname' (the name of the module prepended by `lib') in the cdr
  ;; field.  For example, if MODULE-NAME is the list (inet tcp-ip udp), then
  ;; SUBDIR-AND-LIBNAME will be the pair ("inet/tcp-ip" . "libudp").
  (let ((subdir-and-libname
	 (let loop ((dirs "")
		    (syms module-name))
	   (if (null? (cdr syms))
	       (cons dirs (string-append "lib" (car syms)))
	       (loop (string-append dirs (car syms) "/") (cdr syms)))))
	(init (make-init-name (apply string-append
				     (map (lambda (s)
					    (string-append "_" s))
					  module-name)))))
    (let ((subdir (car subdir-and-libname))
	  (libname (cdr subdir-and-libname)))

      ;; Now look in each dir in %LOAD-PATH for `subdir/libfoo.la'.  If that
      ;; file exists, fetch the dlname from that file and attempt to link
      ;; against it.  If `subdir/libfoo.la' does not exist, or does not seem
      ;; to name any shared library, look for `subdir/libfoo.so' instead and
      ;; link against that.
      (let check-dirs ((dir-list %load-path))
	(if (null? dir-list)
	    #f
	    (let* ((dir (in-vicinity (car dir-list) subdir))
		   (sharlib-full
		    (or (try-using-libtool-name dir libname)
			(try-using-sharlib-name dir libname))))
	      (if (and sharlib-full (file-exists? sharlib-full))
		  (link-dynamic-module sharlib-full init)
		  (check-dirs (cdr dir-list)))))))))

(define (try-using-libtool-name libdir libname)
  (let ((libtool-filename (in-vicinity libdir
				       (string-append libname ".la"))))
    (and (file-exists? libtool-filename)
	 libtool-filename)))
			      
(define (try-using-sharlib-name libdir libname)
  (in-vicinity libdir (string-append libname ".so")))

(define (link-dynamic-module filename initname)
  ;; Register any linked modules which has been registered on the C level
  (register-modules #f)
  (let ((dynobj (dynamic-link filename)))
    (dynamic-call initname dynobj)
    (register-modules dynobj)))

(define (try-module-linked module-name)
  (init-dynamic-module module-name))

(define (try-module-dynamic-link module-name)
  (and (find-and-link-dynamic-module module-name)
       (init-dynamic-module module-name)))



(define autoloads-done '((guile . guile)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
		(member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
	  (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
	(set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
	  (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
	(set! autoloads-done (delete! n autoloads-done))
	(set! autoloads-in-progress (delete! n autoloads-in-progress)))))





;;; {Macros}
;;;

(define (primitive-macro? m)
  (and (macro? m)
       (not (macro-transformer m))))

;;; {Defmacros}
;;;
(define macro-table (make-weak-key-hash-table 523))
(define xformer-table (make-weak-key-hash-table 523))

(define (defmacro? m)  (hashq-ref macro-table m))
(define (assert-defmacro?! m) (hashq-set! macro-table m #t))
(define (defmacro-transformer m) (hashq-ref xformer-table m))
(define (set-defmacro-transformer! m t) (hashq-set! xformer-table m t))

(define defmacro:transformer
  (lambda (f)
    (let* ((xform (lambda (exp env)
		    (copy-tree (apply f (cdr exp)))))
	   (a (procedure->memoizing-macro xform)))
      (assert-defmacro?! a)
      (set-defmacro-transformer! a f)
      a)))


(define defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    (let ((transformer `(lambda ,parms ,@body)))
	      `(define ,name
		 (,(lambda (transformer)
		     (defmacro:transformer transformer))
		  ,transformer))))))
    (defmacro:transformer defmacro-transformer)))

(define defmacro:syntax-transformer
  (lambda (f)
    (procedure->syntax
	      (lambda (exp env)
		(copy-tree (apply f (cdr exp)))))))


;; XXX - should the definition of the car really be looked up in the
;; current module?

(define (macroexpand-1 e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
		(if (defmacro? val)
		    (apply (defmacro-transformer val) (cdr e))
		    e)))
   (#t e)))

(define (macroexpand e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
		(if (defmacro? val)
		    (macroexpand (apply (defmacro-transformer val) (cdr e)))
		    e)))
   (#t e)))

(define (gentemp)
  (gensym "scm:G"))

(provide 'defmacro)



;;; {Run-time options}

((let* ((names '((eval-options-interface
		  (eval-options eval-enable eval-disable)
		  (eval-set!))
		 
		 (debug-options-interface
		  (debug-options debug-enable debug-disable)
		  (debug-set!))
	       
		 (evaluator-traps-interface
		  (traps trap-enable trap-disable)
		  (trap-set!))
		
		 (read-options-interface
		  (read-options read-enable read-disable)
		  (read-set!))
		
		 (print-options-interface
		  (print-options print-enable print-disable)
		  (print-set!))

		 (readline-options-interface
		  (readline-options readline-enable readline-disable)
		  (readline-set!))
		 ))
	(option-name car)
	(option-value cadr)
	(option-documentation caddr)

	(print-option (lambda (option)
			(display (option-name option))
			(if (< (string-length
				(symbol->string (option-name option)))
			       8)
			    (display #\tab))
			(display #\tab)
			(display (option-value option))
			(display #\tab)
			(display (option-documentation option))
			(newline)))

	;; Below follows the macros defining the run-time option interfaces.

	(make-options (lambda (interface)
			`(lambda args
			   (cond ((null? args) (,interface))
				 ((list? (car args))
				  (,interface (car args)) (,interface))
				 (else (for-each ,print-option
						 (,interface #t)))))))

	(make-enable (lambda (interface)
		       `(lambda flags
			  (,interface (append flags (,interface)))
			  (,interface))))

	(make-disable (lambda (interface)
			`(lambda flags
			   (let ((options (,interface)))
			     (for-each (lambda (flag)
					 (set! options (delq! flag options)))
				       flags)
			     (,interface options)
			     (,interface)))))

	(make-set! (lambda (interface)
		     `((name exp)
		       (,'quasiquote
			(begin (,interface (append (,interface)
						   (list '(,'unquote name)
							 (,'unquote exp))))
			       (,interface))))))
	)
   (procedure->macro
     (lambda (exp env)
       (cons 'begin
	     (apply append
		    (map (lambda (group)
			   (let ((interface (car group)))
			     (append (map (lambda (name constructor)
					    `(define ,name
					       ,(constructor interface)))
					  (cadr group)
					  (list make-options
						make-enable
						make-disable))
				     (map (lambda (name constructor)
					    `(defmacro ,name
					       ,@(constructor interface)))
					  (caddr group)
					  (list make-set!)))))
			 names)))))))



;;; {Running Repls}
;;;

(define (repl read evaler print)
  (let loop ((source (read (current-input-port))))
    (print (evaler source))
    (loop (read (current-input-port)))))

;; A provisional repl that acts like the SCM repl:
;;
(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define *unspecified* (if #f #f))
(define (unspecified? v) (eq? v *unspecified*))

(define scm-repl-print-unspecified #f)
(define (assert-repl-print-unspecified v) (set! scm-repl-print-unspecified v))

(define scm-repl-verbose #f)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt "guile> ")

(define (set-repl-prompt! v) (set! scm-repl-prompt v))

(define (default-lazy-handler key . args)
  (save-stack lazy-handler-dispatch)
  (apply throw key args))

(define enter-frame-handler default-lazy-handler)
(define apply-frame-handler default-lazy-handler)
(define exit-frame-handler default-lazy-handler)

(define (lazy-handler-dispatch key . args)
  (case key
    ((apply-frame)
     (apply apply-frame-handler key args))
    ((exit-frame)
     (apply exit-frame-handler key args))
    ((enter-frame)
     (apply enter-frame-handler key args))
    (else
     (apply default-lazy-handler key args))))

(define abort-hook (make-hook))

;; these definitions are used if running a script.
;; otherwise redefined in error-catching-loop.
(define (set-batch-mode?! arg) #t)
(define (batch-mode?) #t)

(define (error-catching-loop thunk)
  (let ((status #f)
	(interactive #t))
    (define (loop first)
      (let ((next 
	     (catch #t

		    (lambda ()
		      (lazy-catch #t
				  (lambda ()
				    (dynamic-wind
				     (lambda () (unmask-signals))
				     (lambda ()
				       (with-traps
					(lambda ()
					  (first)
				       
					  ;; This line is needed because mark
					  ;; doesn't do closures quite right.
					  ;; Unreferenced locals should be
					  ;; collected.
					  ;;
					  (set! first #f)
					  (let loop ((v (thunk)))
					    (loop (thunk)))
					  #f)))
				     (lambda () (mask-signals))))

				  lazy-handler-dispatch))
		    
		    (lambda (key . args)
		      (case key
			((quit)
			 (set! status args)
			 #f)

			((switch-repl)
			 (apply throw 'switch-repl args))

			((abort)
			 ;; This is one of the closures that require
			 ;; (set! first #f) above
			 ;;
			 (lambda ()
			   (run-hook abort-hook)
			   (force-output (current-output-port))
			   (display "ABORT: "  (current-error-port))
			   (write args (current-error-port))
			   (newline (current-error-port))
			   (if interactive
			       (begin
				 (if (and
				      (not has-shown-debugger-hint?)
				      (not (memq 'backtrace
						 (debug-options-interface)))
				      (stack? (fluid-ref the-last-stack)))
				     (begin
				       (newline (current-error-port))
				       (display
					"Type \"(backtrace)\" to get more information or \"(debug)\" to enter the debugger.\n"
					(current-error-port))
				       (set! has-shown-debugger-hint? #t)))
				 (force-output (current-error-port)))
			       (begin
				 (primitive-exit 1)))
			   (set! stack-saved? #f)))

			(else
			 ;; This is the other cons-leak closure...
			 (lambda ()
			   (cond ((= (length args) 4)
				  (apply handle-system-error key args))
				 (else
				  (apply bad-throw key args))))))))))
	(if next (loop next) status)))
    (set! set-batch-mode?! (lambda (arg)
			     (cond (arg 
				    (set! interactive #f)
				    (restore-signals))
				   (#t
				    (error "sorry, not implemented")))))
    (set! batch-mode? (lambda () (not interactive)))
    (loop (lambda () #t))))

;;(define the-last-stack (make-fluid)) Defined by scm_init_backtrace ()
(define before-signal-stack (make-fluid))
(define stack-saved? #f)

(define (save-stack . narrowing)
  (or stack-saved?
      (cond ((not (memq 'debug (debug-options-interface)))
	     (fluid-set! the-last-stack #f)
	     (set! stack-saved? #t))
	    (else
	     (fluid-set!
	      the-last-stack
	      (case (stack-id #t)
		((repl-stack)
		 (apply make-stack #t save-stack eval #t 0 narrowing))
		((load-stack)
		 (apply make-stack #t save-stack 0 #t 0 narrowing))
		((tk-stack)
		 (apply make-stack #t save-stack tk-stack-mark #t 0 narrowing))
		((#t)
		 (apply make-stack #t save-stack 0 1 narrowing))
		(else
		 (let ((id (stack-id #t)))
		   (and (procedure? id)
			(apply make-stack #t save-stack id #t 0 narrowing))))))
	     (set! stack-saved? #t)))))

(define before-error-hook (make-hook))
(define after-error-hook (make-hook))
(define before-backtrace-hook (make-hook))
(define after-backtrace-hook (make-hook))

(define has-shown-debugger-hint? #f)

(define (handle-system-error key . args)
  (let ((cep (current-error-port)))
    (cond ((not (stack? (fluid-ref the-last-stack))))
	  ((memq 'backtrace (debug-options-interface))
	   (run-hook before-backtrace-hook)
	   (newline cep)
	   (display "Backtrace:\n")
	   (display-backtrace (fluid-ref the-last-stack) cep)
	   (newline cep)
	   (run-hook after-backtrace-hook)))
    (run-hook before-error-hook)
    (apply display-error (fluid-ref the-last-stack) cep args)
    (run-hook after-error-hook)
    (force-output cep)
    (throw 'abort key)))

(define (quit . args)
  (apply throw 'quit args))

(define exit quit)

;;(define has-shown-backtrace-hint? #f) Defined by scm_init_backtrace ()

;; Replaced by C code:
;;(define (backtrace)
;;  (if (fluid-ref the-last-stack)
;;      (begin
;;	(newline)
;;	(display-backtrace (fluid-ref the-last-stack) (current-output-port))
;;	(newline)
;;	(if (and (not has-shown-backtrace-hint?)
;;		 (not (memq 'backtrace (debug-options-interface))))
;;	    (begin
;;	      (display
;;"Type \"(debug-enable 'backtrace)\" if you would like a backtrace
;;automatically if an error occurs in the future.\n")
;;	      (set! has-shown-backtrace-hint? #t))))
;;      (display "No backtrace available.\n")))

(define (error-catching-repl r e p)
  (error-catching-loop (lambda () (p (e (r))))))

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define before-read-hook (make-hook))
(define after-read-hook (make-hook))

;;; The default repl-reader function.  We may override this if we've
;;; the readline library.
(define repl-reader
  (lambda (prompt)
    (display prompt)
    (force-output)
    (run-hook before-read-hook)
    (read (current-input-port))))

(define (scm-style-repl)
  (letrec (
	   (start-gc-rt #f)
	   (start-rt #f)
	   (repl-report-start-timing (lambda ()
				       (set! start-gc-rt (gc-run-time))
				       (set! start-rt (get-internal-run-time))))
	   (repl-report (lambda ()
			  (display ";;; ")
			  (display (inexact->exact
				    (* 1000 (/ (- (get-internal-run-time) start-rt)
					       internal-time-units-per-second))))
			  (display "  msec  (")
			  (display  (inexact->exact
				     (* 1000 (/ (- (gc-run-time) start-gc-rt)
						internal-time-units-per-second))))
			  (display " msec in gc)\n")))

	   (consume-trailing-whitespace
	    (lambda ()
	      (let ((ch (peek-char)))
		(cond
		 ((eof-object? ch))
		 ((or (char=? ch #\space) (char=? ch #\tab))
		  (read-char)
		  (consume-trailing-whitespace))
		 ((char=? ch #\newline)
		  (read-char))))))
	   (-read (lambda ()
		    (let ((val
			   (let ((prompt (cond ((string? scm-repl-prompt)
						scm-repl-prompt)
					       ((thunk? scm-repl-prompt)
						(scm-repl-prompt))
					       (scm-repl-prompt "> ")
					       (else ""))))
			     (repl-reader prompt))))

		      ;; As described in R4RS, the READ procedure updates the
		      ;; port to point to the first character past the end of
		      ;; the external representation of the object.  This
		      ;; means that it doesn't consume the newline typically
		      ;; found after an expression.  This means that, when
		      ;; debugging Guile with GDB, GDB gets the newline, which
		      ;; it often interprets as a "continue" command, making
		      ;; breakpoints kind of useless.  So, consume any
		      ;; trailing newline here, as well as any whitespace
		      ;; before it.
		      ;; But not if EOF, for control-D.
		      (if (not (eof-object? val))
			  (consume-trailing-whitespace))
		      (run-hook after-read-hook)
		      (if (eof-object? val)
			  (begin
			    (repl-report-start-timing)
			    (if scm-repl-verbose
				(begin
				  (newline)
				  (display ";;; EOF -- quitting")
				  (newline)))
			    (quit 0)))
		      val)))

	   (-eval (lambda (sourc)
		    (repl-report-start-timing)
		    (start-stack 'repl-stack (eval sourc))))

	   (-print (lambda (result)
		     (if (not scm-repl-silent)
			 (begin
			   (if (or scm-repl-print-unspecified
				   (not (unspecified? result)))
			       (begin
				 (write result)
				 (newline)))
			   (if scm-repl-verbose
			       (repl-report))
			   (force-output)))))

	   (-quit (lambda (args)
		    (if scm-repl-verbose
			(begin
			  (display ";;; QUIT executed, repl exitting")
			  (newline)
			  (repl-report)))
		    args))

	   (-abort (lambda ()
		     (if scm-repl-verbose
			 (begin
			   (display ";;; ABORT executed.")
			   (newline)
			   (repl-report)))
		     (repl -read -eval -print))))

    (let ((status (error-catching-repl -read
				       -eval
				       -print)))
      (-quit status))))
  


;;; {IOTA functions: generating lists of numbers}

(define (iota n)
  (let loop ((count (1- n)) (result '()))
    (if (< count 0) result
        (loop (1- count) (cons count result)))))


;;; {While}
;;;
;;; with `continue' and `break'.
;;;

(defmacro while (cond . body)
  `(letrec ((continue (lambda () (or (not ,cond) (begin (begin ,@ body) (continue)))))
	    (break (lambda val (apply throw 'break val))))
     (catch 'break
	    (lambda () (continue))
	    (lambda v (cadr v)))))

;;; {collect}
;;;
;;; Similar to `begin' but returns a list of the results of all constituent
;;; forms instead of the result of the last form.
;;; (The definition relies on the current left-to-right
;;;  order of evaluation of operands in applications.)

(defmacro collect forms
  (cons 'list forms))

;;; {with-fluids}

;; with-fluids is a convenience wrapper for the builtin procedure
;; `with-fluids*'.  The syntax is just like `let':
;;
;;  (with-fluids ((fluid val)
;;                ...)
;;     body)

(defmacro with-fluids (bindings . body)
  `(with-fluids* (list ,@(map car bindings)) (list ,@(map cadr bindings))
		 (lambda () ,@body)))

;;; Environments

(define the-environment
  (procedure->syntax
   (lambda (x e)
     e)))

(define (environment-module env)
  (let ((closure (and (pair? env) (car (last-pair env)))))
    (and closure (procedure-property closure 'module))))



;;; {Macros}
;;;

;; actually....hobbit might be able to hack these with a little
;; coaxing
;;

(defmacro define-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:transformer ,transformer))))


(defmacro define-syntax-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:syntax-transformer ,transformer))))

;;; {Module System Macros}
;;;

(defmacro define-module args
  `(let* ((process-define-module process-define-module)
	  (set-current-module set-current-module)
	  (module (process-define-module ',args)))
     (set-current-module module)
     module))

;; the guts of the use-modules macro.  add the interfaces of the named
;; modules to the use-list of the current module, in order
(define (process-use-modules module-names)
  (for-each (lambda (module-name)
	      (let ((mod-iface (resolve-interface module-name)))
		(or mod-iface
		    (error "no such module" module-name))
		(module-use! (current-module) mod-iface)))
	    (reverse module-names)))

(defmacro use-modules modules
  `(process-use-modules ',modules))

(defmacro use-syntax (spec)
  `(begin
     ,@(if (pair? spec)
	   `((process-use-modules ',(list spec))
	     (set-module-transformer! (current-module)
				      ,(car (last-pair spec))))
	   `((set-module-transformer! (current-module) ,spec)))
     (set! scm:eval-transformer (module-transformer (current-module)))))

(define define-private define)

(defmacro define-public args
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n) n)
     ((pair? n) (defined-name (car n)))
     (else (syntax))))
  (cond
   ((null? args) (syntax))

   (#t (let ((name (defined-name (car args))))
	 `(begin
	    (let ((public-i (module-public-interface (current-module))))
	      ;; Make sure there is a local variable:
	      ;;
	      (module-define! (current-module)
			      ',name
			      (module-ref (current-module) ',name #f))
			       
	      ;; Make sure that local is exported:
	      ;;
	      (module-add! public-i ',name
			   (module-variable (current-module) ',name)))
			       
	    ;; Now (re)define the var normally.  Bernard URBAN
	    ;; suggests we use eval here to accomodate Hobbit; it lets
	    ;; the interpreter handle the define-private form, which
	    ;; Hobbit can't digest.
	    (eval '(define-private ,@ args)))))))



(defmacro defmacro-public args
  (define (syntax)
    (error "bad syntax" (list 'defmacro-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n)	n)
     (else 		(syntax))))
  (cond
   ((null? args)	(syntax))

   (#t 			(let ((name (defined-name (car args))))
			  `(begin
			     (let ((public-i (module-public-interface (current-module))))
			       ;; Make sure there is a local variable:
			       ;;
			       (module-define! (current-module)
					       ',name
					       (module-ref (current-module) ',name #f))
			       
			       ;; Make sure that local is exported:
			       ;;
			       (module-add! public-i ',name (module-variable (current-module) ',name)))
			       
			     ;; Now (re)define the var normally.
			     ;;
			     (defmacro ,@ args))))))


(defmacro export names
  `(let* ((m (current-module))
	  (public-i (module-public-interface m)))
     (for-each (lambda (name)
		 ;; Make sure there is a local variable:
		 (module-define! m name (module-ref m name #f))
		 ;; Make sure that local is exported:
		 (module-add! public-i name (module-variable m name)))
	       ',names)))

(define export-syntax export)




(define load load-module)



;;; {Load emacs interface support if emacs option is given.}

(define (load-emacs-interface)
  (if (memq 'debug-extensions *features*)
      (debug-enable 'backtrace))
  (define-module (guile-user) :use-module (ice-9 emacs)))



(define using-readline?
  (let ((using-readline? (make-fluid)))
     (make-procedure-with-setter
      (lambda () (fluid-ref using-readline?))
      (lambda (v) (fluid-set! using-readline? v)))))

;; this is just (scm-style-repl) with a wrapper to install and remove 
;; signal handlers.
(define (top-repl) 

  ;; Load emacs interface support if emacs option is given.
  (if (and (module-defined? the-root-module 'use-emacs-interface)
	   use-emacs-interface)
      (load-emacs-interface))

  ;; Place the user in the guile-user module.
  (define-module (guile-user)
    :use-module (guile) ;so that bindings will be checked here first
    :use-module (ice-9 session)
    :use-module (ice-9 debug)
    :autoload (ice-9 debugger) (debug))	;load debugger on demand
  (if (memq 'threads *features*)
      (define-module (guile-user) :use-module (ice-9 threads)))
  (if (memq 'regex *features*)
      (define-module (guile-user) :use-module (ice-9 regex)))

  (let ((old-handlers #f)
	(signals (if (provided? 'posix)
		     `((,SIGINT . "User interrupt")
		       (,SIGFPE . "Arithmetic error")
		       (,SIGSEGV .
				 "Bad memory access (Segmentation violation)"))
		     '())))

    (dynamic-wind

     ;; call at entry
     (lambda ()
       (let ((make-handler (lambda (msg)
			     (lambda (sig)
			       ;; Make a backup copy of the stack
			       (fluid-set! before-signal-stack
					   (fluid-ref the-last-stack))
			       (save-stack %deliver-signals)
			       (scm-error 'signal
					  #f
					  msg
					  #f
					  (list sig))))))
	 (set! old-handlers
	       (map (lambda (sig-msg)
		      (sigaction (car sig-msg)
				 (make-handler (cdr sig-msg))))
		    signals))))

     ;; the protected thunk.
     (lambda ()
       (let ((status (scm-style-repl)))
	 (run-hook exit-hook)
	 status))

     ;; call at exit.
     (lambda ()
       (map (lambda (sig-msg old-handler)
	      (if (not (car old-handler))
		  ;; restore original C handler.
		  (sigaction (car sig-msg) #f)
		  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
		  (sigaction (car sig-msg)
			     (car old-handler)
			     (cdr old-handler))))
	    signals old-handlers)))))

(defmacro false-if-exception (expr)
  `(catch #t (lambda () ,expr)
	  (lambda args #f)))

;;; This hook is run at the very end of an interactive session.
;;;
(define exit-hook (make-hook))


(define-module (guile))

(append! %load-path (cons "." ()))
