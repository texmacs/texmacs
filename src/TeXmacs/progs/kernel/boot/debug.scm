
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : debug.scm
;; DESCRIPTION : debugging tools
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot debug)
  (:export
    display* display-err display-err* tm-display-error
    write* write-err write-err* benchmark write-diff
    texmacs-error check-arg-type check-arg-number check-arg-range
    regression-test-equal regression-test-nequal
    regression-test-group regtest-table-library
    wrap-catch wrap-catch-list
    trace-variables trace-display
    wrap-trace set-trace-level!
    wrap-trace-point set-trace-point!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display* . l)
  "Display all objects in @l."
  (for-each display l))

(define (display-err x)
  "Display @x to the error port."
  (display x (current-error-port)))

(define (display-err* . l)
  "Display all objects in @l to the error port."
  (for-each display-err l))

(define (tm-display-error . l)
  (apply display-err* `("TeXmacs] " ,@l "\n")))

(define (write* . l)
  "Write all objects in @l to standard output."
  (for-each write l))

(define (write-err x)
  "Write @x to the error port."
  (write x (current-error-port)))

(define (write-err* . l)
  "Write all objects in @l to the error port."
  (for-each write-err l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (benchmark message . args)
  `(let ((start (texmacs-time)))
     (begin ,@args)
     (display* ,message " " (- (texmacs-time) start) "msec\n")))

(define (write-diff t u)
  (cond ((== t u) (noop))
	((or (not (and (pair? t) (pair? u))) (not (= (length t) (length u))))
	 (display "< ")
	 (write t)
	 (display "\n> ")
	 (write u)
	 (display "\n"))
	(else
	 (write-diff (car t) (car u))
	 (write-diff (cdr t) (cdr u)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs errors and assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define old-format?
  (catch 'wrong-number-of-args
	 (lambda () (car))
	 (lambda (type caller message opts extra)
	   (let next ((l (string->list message)))
	     (cond ((null? l) #f)
		   ((char=? #\% (car l)) #t)
		   (else (next (cdr l))))))))

(define (scm-error* type caller message . opt)
  (if old-format?
      (begin (set! message (string-replace message "~S" "%S"))
	     (set! message (string-replace message "~A" "%s"))))
  (apply scm-error type caller message opt))

(define (texmacs-error where message . args)
  (scm-error* 'texmacs-error where message args #f))

(define (check-arg-type pred arg caller)
  (if (pred arg) arg
      (scm-error* 'wrong-type-arg caller
		  "Wrong type argument: ~S" (list arg) '())))
  
(define (check-arg-number pred num caller)
  (if (pred num) num
      (scm-error* 'wrong-number-of-args caller
		  "Wrong number of arguments: ~A" (list num) '())))

(define (check-arg-range pred arg caller)
  (if (pred arg) arg
      (scm-error* 'out-of-range caller
		  "Argument out of range: ~S" (list arg) '())))

(define (syntax-error where message . args)
  (scm-error* 'syntax-error where message args #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regression testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regression-test-equal group test res-in result exp-in expected)
  (if (not (equal? result expected))
      (begin
	(newline)
	(display* "Expected in - " (object->string exp-in) "\n")
	(display* "Result   in - " (object->string res-in) "\n")
	(display* "Expected - " (object->string expected) "\n")
	(display* "Result   - " (object->string result) "\n")
	(display* "Do not match!\n")
	(display* "Regression failure: " group " / " test)
	(error "Regression failure:" group test))))

(define (regression-test-nequal group test res-in result exp-in expected)
  (if (equal? result expected)
      (begin
	(newline)
	(display* "Expected Not in - " (object->string exp-in) "\n")
	(display* "Result       in - " (object->string res-in) "\n")
	(display* "Expected Not - " (object->string expected) "\n")
	(display* "Result       - " (object->string result) "\n")
	(display* "Unwanted match! Regression failure!\n")
	(display* "Regression failure: " group " / " test)
	(error "Regression failure:" group test))))

(define-macro (regression-test-group group-desc group-id
				     result-cmd expected-cmd . body)
  (let* ((make-command (lambda (cmd)
			 (if (equal? cmd ':none)
			     (lambda (x) x)
			     (lambda (x) (list cmd x)))))
	 (make-result (make-command result-cmd))
	 (make-expected (make-command expected-cmd))
	 (tests
	  (let rec ((n 1) (l body))	; process body items
	    (define (check-test)
	      (let ((t (first l)))
		(if (null? (cdr t))
		    (error "empty test in group " group-id))
		(let ((test-desc (second t)))
		  (check-arg-type string? test-desc group-id)
		  (check-arg-number (lambda (x) (equal? 4 x)) (length t)
				    (string-append group-id "/" test-desc)))))
	    (define (make-test e?)
	      (check-test)
	      (let* ((t (first l))
		     (test-desc (second t))
		     (result-in (third t))
		     (expected-in (fourth t))
		     (result (make-result result-in))
		     (expected (make-expected expected-in)))
		;; Display messages and run test.
		`((display ,(string-append "  -- " test-desc "\n"))
		  (,(if e? 'regression-test-equal 'regression-test-nequal)
		   ,group-id ,test-desc
		   ,result-in ,result ,expected-in ,expected)
		  ,@(rec (1+ n) (cdr l))))) ; rest of the body
	    (cond ((null? l) `(,(1- n))) ; evaluate to number of tests
		  ;; Improper list or unexpect atom. Nevermind.
		  ((not (pair? l)) l)
		  ((not (pair? (car l)))
		   (cons (car l) (rec n (cdr l))))
		  ;; Test case.
		  ((equal? 'test (caar l)) (make-test #t))
		  ((equal? 'test-fails (caar l)) (make-test #f))
		  ;; Non-test form, preserve.
		  (else (cons (car l) (rec n (cdr l))))))))
    `(begin
       (display ,(string-append "Test group: " group-desc " [" group-id "]\n"))
       ,@tests)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test suite library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (regtest-table-library)
  ;; basic shorthands for input of texmacs tables
  `(begin
     (define (cell x) `(cell ,x))
     (define (row l) `(row ,@(map cell l)))
     (define (table ll) `(table ,@(map row ll)))
     (define (tformat pp ll) `(tformat ,@pp ,(table ll)))
     (define (colwith i var val) `(cwith "1" "-1" ,i ,i ,var ,val))
     (define (rowwith i var val) `(cwith ,i ,i "1" "-1" ,var ,val))
     (define (allwith var val) `(cwith "1" "-1" "1" "-1" ,var ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wrap-catch proc)
  ;; Wrap a procedure in a closure which displays and passes exceptions.
  (lambda args
    (lazy-catch #t
		(lambda () (apply proc args))
		(lambda err
		  (tm-display-error "Guile error: " (list err))
		  (apply throw err)))))

(define (wrap-catch-list expr)
  ;; Similar to wrap-catch for a scheme expression in list form.
  `(lazy-catch #t
	       (lambda () ,expr)
	       (lambda err
		 (tm-display-error "Guile error: " (list err))
		 (apply throw err))))

(define trace-level 0)
(define (trace-indent)
  ;; Produce the string to be used to indent trace output.
  (let rec ((n trace-level) (s '()))
    (if (equal? 0 n) (apply string-append s)
	(rec (1- n) (cons "| " s)))))

(define (trace-display . args)
  ;; As display but also print trace indentation.
  (display (trace-indent))
  (for-each (lambda (a)
	      (display (if (string? a) a (object->string a)))
	      (display " "))
	    args)
  (newline))

(define-macro (trace-variables . vars)
  ;; Use trace-display to show the name and value of some variables.
  (define (trace-one-variable v)
    `(trace-display (string-append ,(symbol->string v) ": "
				   (object->string ,v))))
  `(begin ,@(map trace-one-variable vars)))
				     

;;   Trace levels
;; Display parameters and return value of a function.
;; Increase the trace indentation to show the call hierarchy.
;; Do not preserve tail recursion.

(define (wrap-trace name lam)
  (lambda args
    (trace-display
     (if (null? args)
	 (string-append "[" name "]")
	 (apply string-append
		`("[" ,name
		  ,@(map (lambda (x) (string-append " " (object->string x)))
			 args) "]"))))
    (set! trace-level (1+ trace-level))
    (lazy-catch #t
		(lambda ()
		  (let ((res (apply lam args)))
		    (set! trace-level (1- trace-level))      
		    (trace-display (object->string res))
		    res))
		(lambda err
		  (set! trace-level (1- trace-level))
		  (apply throw err)))))

(define-macro (set-trace-level! . names)
  ;; Make each function a trace-level. Functions can be set multiple
  ;; times, only the first application is effective.
  ;; Parameters are function names
  `(begin
     ,@(map (lambda (name)
	      `(if (not (procedure-property ,name 'trace-wrapped))
		   (begin
		     (set! ,name (wrap-trace ,(symbol->string name) ,name))
		     (set-procedure-property! ,name 'trace-wrapped #t))))
	    names)))

;;   Trace points
;; Display parameters of a function when it is called.
;; Preserve tail recursion.

(define (wrap-trace-point lam msg)
  (lambda args
    (trace-display (string-append "[" msg " " (object->string args) "]"))
    (apply lam args)))

(define-macro (set-trace-point! name . opt)
  ;; Make one trace point.
  ;; Care must be taken of net setting the same function multiple times.
  (let ((msg (if (null? opt)
		 (symbol->string name)
		 (car opt))))
    `(set! ,name (wrap-trace-point ,name ,msg))))
