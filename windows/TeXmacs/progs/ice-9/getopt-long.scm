;;; Author: Russ McManus
;;; $Id$
;;;
;;; Copyright (C) 1998 FSF
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; 
;;; This module implements some complex command line option parsing, in
;;; the spirit of the GNU C library function 'getopt_long'.  Both long
;;; and short options are supported.
;;; 
;;; The theory is that people should be able to constrain the set of
;;; options they want to process using a grammar, rather than some arbitrary
;;; structure.  The grammar makes the option descriptions easy to read.
;;; 

;;; getopt-long is a function for parsing command-line arguments in a
;;; manner consistent with other GNU programs.

;;; (getopt-long ARGS GRAMMAR)
;;; Parse the arguments ARGS according to the argument list grammar GRAMMAR.
;;;
;;; ARGS should be a list of strings.  Its first element should be the
;;; name of the program; subsequent elements should be the arguments
;;; that were passed to the program on the command line.  The
;;; `program-arguments' procedure returns a list of this form.
;;;
;;; GRAMMAR is a list of the form:
;;; ((OPTION (PROPERTY VALUE) ...) ...)
;;;
;;; Each OPTION should be a symbol.  `getopt-long' will accept a
;;; command-line option named `--OPTION'.
;;; Each option can have the following (PROPERTY VALUE) pairs:
;;; 
;;;   (single-char CHAR) --- Accept `-CHAR' as a single-character
;;;		equivalent to `--OPTION'.  This is how to specify traditional
;;;		Unix-style flags.
;;;   (required? BOOL) --- If BOOL is true, the option is required.
;;;		getopt-long will raise an error if it is not found in ARGS.
;;;   (value BOOL) --- If BOOL is #t, the option accepts a value; if
;;;		it is #f, it does not; and if it is the symbol
;;;		`optional', the option may appear in ARGS with or
;;;		without a value. 
;;;   (predicate FUNC) --- If the option accepts a value (i.e. you
;;;		specified `(value #t)' for this option), then getopt
;;;		will apply FUNC to the value, and throw an exception
;;;		if it returns #f.  FUNC should be a procedure which
;;;		accepts a string and returns a boolean value; you may
;;;		need to use quasiquotes to get it into GRAMMAR.
;;;
;;; The (PROPERTY VALUE) pairs may occur in any order, but each
;;; property may occur only once.  By default, options do not have
;;; single-character equivalents, are not required, and do not take
;;; values.
;;; 
;;; In ARGS, single-character options may be combined, in the usual
;;; Unix fashion: ("-x" "-y") is equivalent to ("-xy").  If an option
;;; accepts values, then it must be the last option in the
;;; combination; the value is the next argument.  So, for example, using
;;; the following grammar:
;;;      ((apples    (single-char #\a))
;;;       (blimps    (single-char #\b) (value #t))
;;;       (catalexis (single-char #\c) (value #t)))
;;; the following argument lists would be acceptable:
;;;    ("-a" "-b" "bang" "-c" "couth")     ("bang" and "couth" are the values
;;;                                         for "blimps" and "catalexis")
;;;    ("-ab" "bang" "-c" "couth")         (same)
;;;    ("-ac" "couth" "-b" "bang")         (same)
;;;    ("-abc" "couth" "bang")             (an error, since `-b' is not the
;;;                                         last option in its combination)
;;;
;;; If an option's value is optional, then `getopt-long' decides
;;; whether it has a value by looking at what follows it in ARGS.  If
;;; the next element is a string, and it does not appear to be an
;;; option itself, then that string is the option's value.
;;;
;;; The value of a long option can appear as the next element in ARGS,
;;; or it can follow the option name, separated by an `=' character.
;;; Thus, using the same grammar as above, the following argument lists
;;; are equivalent:
;;;   ("--apples" "Braeburn" "--blimps" "Goodyear")
;;;   ("--apples=Braeburn" "--blimps" "Goodyear")
;;;   ("--blimps" "Goodyear" "--apples=Braeburn")
;;;
;;; If the option "--" appears in ARGS, argument parsing stops there;
;;; subsequent arguments are returned as ordinary arguments, even if
;;; they resemble options.  So, in the argument list:
;;;         ("--apples" "Granny Smith" "--" "--blimp" "Goodyear")
;;; `getopt-long' will recognize the `apples' option as having the
;;; value "Granny Smith", but it will not recognize the `blimp'
;;; option; it will return the strings "--blimp" and "Goodyear" as
;;; ordinary argument strings.
;;;
;;; The `getopt-long' function returns the parsed argument list as an
;;; assocation list, mapping option names --- the symbols from GRAMMAR
;;; --- onto their values, or #t if the option does not accept a value.
;;; Unused options do not appear in the alist.
;;;
;;; All arguments that are not the value of any option are returned
;;; as a list, associated with the empty list.
;;;
;;; `getopt-long' throws an exception if:
;;; - it finds an unrecognized option in ARGS
;;; - a required option is omitted
;;; - an option that requires an argument doesn't get one
;;; - an option that doesn't accept an argument does get one (this can
;;;   only happen using the long option `--opt=value' syntax)
;;; - an option predicate fails
;;;
;;; So, for example:
;;;
;;; (define grammar
;;;   `((lockfile-dir (required? #t)
;;;                   (value #t)
;;;                   (single-char #\k)
;;;                   (predicate ,file-is-directory?))
;;;     (verbose (required? #f)
;;;              (single-char #\v)
;;;              (value #f))
;;;     (x-includes (single-char #\x))
;;;     (rnet-server (single-char #\y) 
;;;                  (predicate ,string?))))
;;;
;;; (getopt-long '("my-prog" "-vk" "/tmp" "foo1" "--x-includes=/usr/include" 
;;;                "--rnet-server=lamprod" "--" "-fred" "foo2" "foo3")
;;;                grammar)
;;; => ((() "foo1" "-fred" "foo2" "foo3")
;;; 	(rnet-server . "lamprod")
;;; 	(x-includes . "/usr/include")
;;; 	(lockfile-dir . "/tmp")
;;; 	(verbose . #t))


(define-module (ice-9 getopt-long)
  :use-module (ice-9 common-list))
;;; end-header


;;; The code on this page was expanded by hand using the following code:
;;; (pretty-print                 
;;;  (macroexpand                 
;;;   '(define-record option-spec 
;;;      (name                    
;;;       value                   
;;;       value-required?         
;;;       single-char             
;;;       predicate-ls            
;;;       parse-ls))))            
;;;
;;; This avoids the need to load slib for records.
(define slib:error error)
(begin (define
         option-spec->name
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 1)
               (slib:error
                 (quote option-spec->name)
                 ": bad record"
                 obj))))
       (define
         option-spec->value
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 2)
               (slib:error
                 (quote option-spec->value)
                 ": bad record"
                 obj))))
       (define
         option-spec->value-required?
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 3)
               (slib:error
                 (quote option-spec->value-required?)
                 ": bad record"
                 obj))))
       (define
         option-spec->single-char
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 4)
               (slib:error
                 (quote option-spec->single-char)
                 ": bad record"
                 obj))))
       (define
         option-spec->predicate-ls
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 5)
               (slib:error
                 (quote option-spec->predicate-ls)
                 ": bad record"
                 obj))))
       (define
         option-spec->parse-ls
         (lambda
           (obj)
           (if (option-spec? obj)
               (vector-ref obj 6)
               (slib:error
                 (quote option-spec->parse-ls)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-name!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 1 val)
               (slib:error
                 (quote set-option-spec-name!)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-value!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 2 val)
               (slib:error
                 (quote set-option-spec-value!)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-value-required?!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 3 val)
               (slib:error
                 (quote set-option-spec-value-required?!)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-single-char!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 4 val)
               (slib:error
                 (quote set-option-spec-single-char!)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-predicate-ls!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 5 val)
               (slib:error
                 (quote set-option-spec-predicate-ls!)
                 ": bad record"
                 obj))))
       (define
         set-option-spec-parse-ls!
         (lambda
           (obj val)
           (if (option-spec? obj)
               (vector-set! obj 6 val)
               (slib:error
                 (quote set-option-spec-parse-ls!)
                 ": bad record"
                 obj))))
       (define
         option-spec?
         (lambda
           (obj)
           (and (vector? obj)
                (= (vector-length obj) 7)
                (eq? (vector-ref obj 0) (quote option-spec)))))
       (define
         make-option-spec
         (lambda
           (option-spec->name
             option-spec->value
             option-spec->value-required?
             option-spec->single-char
             option-spec->predicate-ls
             option-spec->parse-ls)
           (vector
             (quote option-spec)
             option-spec->name
             option-spec->value
             option-spec->value-required?
             option-spec->single-char
             option-spec->predicate-ls
             option-spec->parse-ls))))


;;;
;;; parse functions go on this page.
;;;
(define make-user-predicate
  (lambda (pred)
    (lambda (spec)
      (let ((val (option-spec->value spec)))
	(if (and val
		 (pred val)) #t
		 (error "option predicate failed:" (option-spec->name spec)))))))

(define make-not-allowed-value-fn
  (lambda ()
    (lambda (spec)
      (let ((val (option-spec->value spec)))
	(if (not (or (eq? val #t)
		     (eq? val #f)))
	    (let ((name (option-spec->name spec)))
	      (error "option does not support argument:" name)))))))

(define make-option-required-predicate
  (lambda ()
    (lambda (spec)
      (let ((val (option-spec->value spec)))
	(if (not val)
	    (let ((name (option-spec->name spec)))
	      (error "option must be specified:" name)))))))

(define make-option-value-predicate 
  (lambda (predicate)
    (lambda (spec)
      (let ((val (option-spec->value spec)))
	(if (not (predicate val))
	    (let ((name (option-spec->name spec)))
	      (error "Bad option value:" name val)))))))

(define make-required-value-fn
  (lambda ()
    (lambda (spec)
      (let ((val (option-spec->value spec)))
	(if (eq? val #t)
	    (let ((name (option-spec->name spec)))
	      (error "option must be specified with argument:" name)))))))

(define single-char-value? 
  (lambda (val)
    (char? val)))
 
(define (parse-option-spec desc)
  (letrec ((parse-iter
	    (lambda (spec)
	      (let ((parse-ls (option-spec->parse-ls spec)))
		(if (null? parse-ls)
		    spec
		    (let ((ls (car parse-ls)))
		      (if (or (not (list? ls))
			      (not (= (length ls) 2)))
			  (error "Bad option specification:" ls))
		      (let ((key (car ls))
			    (val (cadr ls)))
			(cond ((and (eq? key 'required?) val)
			       ;; required values are implemented as a predicate
			       (parse-iter (make-option-spec (option-spec->name spec)
							     (option-spec->value spec)
							     (option-spec->value-required? spec)
							     (option-spec->single-char spec)
							     (cons (make-option-required-predicate)
								   (option-spec->predicate-ls spec))
							     (cdr parse-ls))))
			      ;; if the value is not required, then don't add a predicate,
			      ((eq? key 'required?)
			       (parse-iter (make-option-spec (option-spec->name spec)
							     (option-spec->value spec)
							     (option-spec->value-required? spec)
							     (option-spec->single-char spec)
							     (option-spec->predicate-ls spec)
							     (cdr parse-ls))))
			      ;; handle value specification
			      ((eq? key 'value)
			       (cond ((eq? val #t)
				      ;; when value is required, add a predicate to that effect
				      ;; and record the fact in value-required? field.
				      (parse-iter (make-option-spec (option-spec->name spec)
								    (option-spec->value spec)
								    #t
								    (option-spec->single-char spec)
								    (cons (make-required-value-fn) 
									  (option-spec->predicate-ls spec))
								    (cdr parse-ls))))
				     ((eq? val #f)
				      ;; when the value is not allowed, add a predicate to that effect.
				      ;; one can detect that a value is not supplied by checking the option
				      ;; value against #f.
				      (parse-iter (make-option-spec (option-spec->name spec)
								    (option-spec->value spec)
								    #f
								    (option-spec->single-char spec)
								    (cons (make-not-allowed-value-fn) 
									  (option-spec->predicate-ls spec))
								    (cdr parse-ls))))
				     ((eq? val 'optional)
				      ;; for optional values, don't add a predicate.  do, however
				      ;; put the value 'optional in the value-required? field.  this
				      ;; setting checks whether optional values are 'greedy'.  set
				      ;; to #f to make optional value clauses 'non-greedy'.

				      (parse-iter (make-option-spec (option-spec->name spec)
								    (option-spec->value spec)
								    'optional
								    (option-spec->single-char spec)
								    (option-spec->predicate-ls spec)
								    (cdr parse-ls))))
				     (#t
				      ;; error case
				      (error "Bad value specification for option:" (cons key val)))))
			      ;; specify which single char is defined for this option.
			      ((eq? key 'single-char)
			       (if (not (single-char-value? val))
				   (error "Not a single-char-value:" val " for option:" key)
				   (parse-iter (make-option-spec (option-spec->name spec)
								 (option-spec->value spec)
								 (option-spec->value-required? spec)
								 val
								 (option-spec->predicate-ls spec)
								 (cdr parse-ls)))))
			      ((eq? key 'predicate)
			       (if (procedure? val)
				   (parse-iter (make-option-spec (option-spec->name spec)
								 (option-spec->value spec)
								 (option-spec->value-required? spec)
								 (option-spec->single-char spec)
								 (cons (make-user-predicate val)
								       (option-spec->predicate-ls spec))
								 (cdr parse-ls)))
				   (error "Bad predicate specified for option:" (cons key val))))))))))))
    (if (or (not (pair? desc))
	    (string? (car desc)))
	(error "Bad option specification:" desc))
    (parse-iter (make-option-spec (car desc)
				  #f 
				  #f
				  #f
				  '()
				  (cdr desc)))))


;;;
;;; 
;;;
(define (split-arg-list argument-list)
  "Given an ARGUMENT-LIST, decide which part to process for options.  
Everything before an arg of \"--\" is fair game, everything after it 
should not be processed.  The \"--\" is discarded.  A cons pair is 
returned whose car is the list to process for options, and whose cdr 
is the list to not process."
  (let loop ((process-ls '())
	     (not-process-ls argument-list))
    (cond ((null? not-process-ls)
	   (cons (reverse process-ls) '()))
	  ((string=? "--" (car not-process-ls))
	   (cons (reverse process-ls) (cdr not-process-ls)))
	  (#t
	   (loop (cons (car not-process-ls) process-ls)
		 (cdr not-process-ls))))))

(define short-opt-rx (make-regexp "^-([a-zA-Z]+)"))
(define long-opt-no-value-rx (make-regexp "^--([^=]+)$"))
(define long-opt-with-value-rx (make-regexp "^--([^=]+)=(.*)"))

(define (single-char-expander specifications opt-ls)
  "Expand single letter options that are mushed together."
  (let ((response #f))
    (define (is-short-opt? str)
      (set! response (regexp-exec short-opt-rx str))
      response)
    (define (iter opt-ls ret-ls)
      (cond ((null? opt-ls)
	     (reverse ret-ls))
	    ((is-short-opt? (car opt-ls))
	     (let* ((orig-str (car opt-ls))
		    (match-pair (vector-ref response 2))
		    (match-str (substring orig-str (car match-pair) (cdr match-pair))))
	       (if (= (string-length match-str) 1)
		   (iter (cdr opt-ls)
			 (cons (string-append "-" match-str) ret-ls))
		   (iter (cons (string-append "-" (substring match-str 1)) (cdr opt-ls))
			 (cons (string-append "-" (substring match-str 0 1)) ret-ls)))))
	    (#t (iter (cdr opt-ls)
		      (cons (car opt-ls) ret-ls)))))
    (iter opt-ls '())))

(define (process-short-option specifications argument-ls alist)
  "Process a single short option that appears at the front of the ARGUMENT-LS,
according to SPECIFICATIONS.  Returns #f is there is no such argument.  Otherwise 
returns a pair whose car is the list of remaining arguments, and whose cdr is a 
new association list, constructed by adding a pair to the supplied ALIST.  
The pair on the front of the returned association list describes the  option 
found at the head of ARGUMENT-LS.  The way this routine currently works, an 
option that never takes a value that is followed by a non option will cause 
an error, which is probably a bug.  To fix the bug the option specification
needs to record whether the option ever can take a value."
  (define (short-option->char option)
    (string-ref option 1))
  (define (is-short-option? option)
    (regexp-exec short-opt-rx option))
  (define (is-long-option? option)
    (or (regexp-exec long-opt-with-value-rx option)
	(regexp-exec long-opt-no-value-rx option)))
  (define (find-matching-spec option)
    (let ((key (short-option->char option)))
      (find-if (lambda (spec) (eq? key (option-spec->single-char spec))) specifications)))
  (let ((option (car argument-ls)))
    (if (is-short-option? option)
	(let ((spec (find-matching-spec option)))
	  (if spec
	      (let* ((next-value (if (null? (cdr argument-ls)) #f (cadr argument-ls)))
		     (option-value (if (and next-value
					    (not (is-short-option? next-value))
					    (not (is-long-option? next-value))
					    (option-spec->value-required? spec))
				       next-value
				       #t))
		     (new-alist (cons (cons (option-spec->name spec) option-value) alist)))
		(cons (if (eq? option-value #t)
			  (cdr argument-ls)   ; there was one value specified, skip just one
			  (cddr argument-ls)) ; there must have been a value specified, skip two
		      new-alist))
	      (error "No such option:" option)))
	#f)))

(define (process-long-option specifications argument-ls alist)
  (define (find-matching-spec key)
    (find-if (lambda (spec) (eq? key (option-spec->name spec))) specifications))
  (define (split-long-option option)
    ;; returns a pair whose car is a symbol naming the option, cdr is
    ;; the option value.  as a special case, if the option value is
    ;; #f, then the caller should use the next item in argument-ls as
    ;; the option value.
    (let ((resp (regexp-exec long-opt-no-value-rx option)))
      (if resp
	  ;; Aha, we've found a long option without an equal sign.
	  ;; Maybe we need to grab a value from argument-ls.  To find
	  ;; out we need to refer to the option-spec.
	  (let* ((key-pair (vector-ref resp 2))
		 (key (string->symbol (substring option (car key-pair) (cdr key-pair))))
		 (spec (find-matching-spec key)))
	    (cons key (if (option-spec->value-required? spec) #f #t)))
	  (let ((resp (regexp-exec long-opt-with-value-rx option)))
	    ;; Aha, we've found a long option with an equal sign.  The
	    ;; option value is simply the value to the right of the
	    ;; equal sign.
	    (if resp
		(let* ((key-pair (vector-ref resp 2))
		       (key (string->symbol (substring option (car key-pair) (cdr key-pair))))
		       (value-pair (vector-ref resp 3))
		       (value (substring option (car value-pair) (cdr value-pair))))
		  (cons key value))
		  #f)))))
  (let* ((option (car argument-ls))
	 (pair (split-long-option option)))
    (cond ((and pair (eq? (cdr pair) #f))
	   (if (null? (cdr argument-ls))
	       (error "Not enough options.")
	       (cons (cddr argument-ls)
		     (cons (cons (car pair) (cadr argument-ls)) alist))))
	  (pair
	   (cons (cdr argument-ls) (cons pair alist)))
	  (else #f))))

(define (process-options specifications argument-ls)
  (define (iter argument-ls alist rest-ls)
    (if (null? argument-ls)
	(cons alist (reverse rest-ls))
	(let ((pair (process-short-option specifications argument-ls alist)))
	  (if pair
	      (let ((argument-ls (car pair))
		    (alist (cdr pair)))
		(iter argument-ls alist rest-ls))
	      (let ((pair (process-long-option specifications argument-ls alist)))
		(if pair
		    (let ((argument-ls (car pair))
			  (alist (cdr pair)))
		      (iter argument-ls alist rest-ls))
		    (iter (cdr argument-ls)
			  alist
			  (cons (car argument-ls) rest-ls))))))))
  (iter argument-ls '() '()))

(define (getopt-long program-arguments option-desc-list)
  "Process options, handling both long and short options, similar to
the glibc function 'getopt_long'.  PROGRAM-ARGUMENTS should be a value
similar to what (program-arguments) returns.  OPTION-DESC-LIST is a
list of option descriptions.  Each option description must satisfy the
following grammar:

    <option-spec>           :: (<name> . <attribute-ls>)
    <attribute-ls>          :: (<attribute> . <attribute-ls>)
                               | ()
    <attribute>             :: <required-attribute>
                               | <arg-required-attribute>
                               | <single-char-attribute>
                               | <predicate-attribute>
                               | <value-attribute>
    <required-attribute>    :: (required? <boolean>)
    <single-char-attribute> :: (single-char <char>)
    <value-attribute>       :: (value #t)
                               (value #f)
                               (value optional)
    <predicate-attribute>   :: (predicate <1-ary-function>)

    The procedure returns an alist of option names and values.  Each
option name is a symbol.  The option value will be '#t' if no value
was specified.  There is a special item in the returned alist with a
key of the empty list, (): the list of arguments that are not options
or option values.
    By default, options are not required, and option values are not 
required.  By default, single character equivalents are not supported;
if you want to allow the user to use single character options, you need
to add a 'single-char' clause to the option description."
  (let* ((specifications (map parse-option-spec option-desc-list))
	 (pair (split-arg-list (cdr program-arguments)))
	 (split-ls (single-char-expander specifications (car pair)))
	 (non-split-ls (cdr pair)))
    (let* ((opt-pair (process-options specifications split-ls))
	   (alist (car opt-pair))
	   (rest-ls (append (cdr opt-pair) non-split-ls)))
      ;; loop through the returned alist, and set the values into the specifications
      (for-each (lambda (pair)
		  (let* ((key (car pair))
			 (val (cdr pair))
			 (spec (find-if (lambda (spec) (eq? key (option-spec->name spec)))
					specifications)))
		    (if spec (set-option-spec-value! spec val))))
		alist)
      ;; now fire all the predicates
      (for-each (lambda (spec)
		  (let ((predicate-ls (option-spec->predicate-ls spec)))
		    (for-each (lambda (predicate)
				(predicate spec))
			      predicate-ls)))
		specifications)
      (cons (cons '() rest-ls) alist))))

(define (option-ref options key default)
  "Look for an option value in OPTIONS using KEY.  If no such value is
found, return DEFAULT."
  (let ((pair (assq key options)))
    (if pair
	(cdr pair)
	default)))

(export option-ref)
(export getopt-long)
