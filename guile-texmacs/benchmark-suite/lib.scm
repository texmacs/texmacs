;;;; benchmark-suite/lib.scm --- generic support for benchmarking
;;;; Copyright (C) 2002, 2006 Free Software Foundation, Inc.
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
;;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA

(define-module (benchmark-suite lib)
  :export (

 ;; Controlling the execution.
 iteration-factor
 scale-iterations

 ;; Running benchmarks.
 run-benchmark
 benchmark

 ;; Naming groups of benchmarks in a regular fashion.
 with-benchmark-prefix with-benchmark-prefix* current-benchmark-prefix
 format-benchmark-name

 ;; Computing timing results
 benchmark-time-base
 benchmark-total-time benchmark-user-time benchmark-system-time
 benchmark-frame-time benchmark-core-time
 benchmark-user-time\interpreter benchmark-core-time\interpreter

 ;; Reporting results in various ways.
 register-reporter unregister-reporter reporter-registered?
 make-log-reporter
 full-reporter
 user-reporter))


;;;; If you're using Emacs's Scheme mode:
;;;;   (put 'with-benchmark-prefix 'scheme-indent-function 1)
;;;;   (put 'benchmark 'scheme-indent-function 1)


;;;; CORE FUNCTIONS
;;;;
;;;; The function (run-benchmark name iterations thunk) is the heart of the
;;;; benchmarking environment.  The first parameter NAME is a unique name for
;;;; the benchmark to be executed (for an explanation of this parameter see
;;;; below under ;;;; NAMES.  The second parameter ITERATIONS is a positive
;;;; integer value that indicates how often the thunk shall be executed (for
;;;; an explanation of how iteration counts should be used, see below under
;;;; ;;;; ITERATION COUNTS).  For example:
;;;;
;;;;    (run-benchmark "small integer addition" 100000 (lambda () (+ 1 1)))
;;;;
;;;; This will run the function (lambda () (+ 1 1)) a 100000 times (the
;;;; iteration count can, however be scaled.  See below for details).  Some
;;;; different time data for running the thunk for the given number of
;;;; iterations is measured and reported.
;;;;
;;;; Convenience macro
;;;;
;;;; * (benchmark name iterations body) is a short form for
;;;;   (run-benchmark name iterations (lambda () body))


;;;; NAMES
;;;;
;;;; Every benchmark in the benchmark suite has a unique name to be able to
;;;; compare the results of individual benchmarks across several runs of the
;;;; benchmark suite.
;;;;
;;;; A benchmark name is a list of printable objects.  For example:
;;;; ("ports.scm" "file" "read and write back list of strings")
;;;; ("ports.scm" "pipe" "read")
;;;;
;;;; Benchmark names may contain arbitrary objects, but they always have
;;;; the following properties:
;;;; - Benchmark names can be compared with EQUAL?.
;;;; - Benchmark names can be reliably stored and retrieved with the standard
;;;;   WRITE and READ procedures; doing so preserves their identity.
;;;;
;;;; For example:
;;;;
;;;;    (benchmark "simple addition" 100000 (+ 2 2))
;;;;
;;;; In that case, the benchmark name is the list ("simple addition").
;;;;
;;;; The WITH-BENCHMARK-PREFIX syntax and WITH-BENCHMARK-PREFIX* procedure
;;;; establish a prefix for the names of all benchmarks whose results are
;;;; reported within their dynamic scope.  For example:
;;;;
;;;; (begin
;;;;   (with-benchmark-prefix "basic arithmetic"
;;;;     (benchmark "addition" 100000 (+ 2 2))
;;;;     (benchmark "subtraction" 100000 (- 4 2)))
;;;;   (benchmark "multiplication" 100000 (* 2 2))))
;;;;
;;;; In that example, the three benchmark names are:
;;;;   ("basic arithmetic" "addition"),
;;;;   ("basic arithmetic" "subtraction"), and
;;;;   ("multiplication").
;;;;
;;;; WITH-BENCHMARK-PREFIX can be nested.  Each WITH-BENCHMARK-PREFIX
;;;; postpends a new element to the current prefix:
;;;;
;;;; (with-benchmark-prefix "arithmetic"
;;;;   (with-benchmark-prefix "addition"
;;;;     (benchmark "integer" 100000 (+ 2 2))
;;;;     (benchmark "complex" 100000 (+ 2+3i 4+5i)))
;;;;   (with-benchmark-prefix "subtraction"
;;;;     (benchmark "integer" 100000 (- 2 2))
;;;;     (benchmark "complex" 100000 (- 2+3i 1+2i))))
;;;;
;;;; The four benchmark names here are:
;;;;   ("arithmetic" "addition" "integer")
;;;;   ("arithmetic" "addition" "complex")
;;;;   ("arithmetic" "subtraction" "integer")
;;;;   ("arithmetic" "subtraction" "complex")
;;;;
;;;; To print a name for a human reader, we DISPLAY its elements,
;;;; separated by ": ".  So, the last set of benchmark names would be
;;;; reported as:
;;;;
;;;;   arithmetic: addition: integer
;;;;   arithmetic: addition: complex
;;;;   arithmetic: subtraction: integer
;;;;   arithmetic: subtraction: complex
;;;;
;;;; The Guile benchmarks use with-benchmark-prefix to include the name of
;;;; the source file containing the benchmark in the benchmark name, to
;;;; provide each file with its own namespace.


;;;; ITERATION COUNTS
;;;;
;;;; Every benchmark has to be given an iteration count that indicates how
;;;; often it should be executed.  The reason is, that in most cases a single
;;;; execution of the benchmark code would not deliver usable timing results:
;;;; The resolution of the system time is not arbitrarily fine.  Thus, some
;;;; benchmarks would be executed too quickly to be measured at all.  A rule
;;;; of thumb is, that the longer a benchmark runs, the more exact is the
;;;; information about the execution time.
;;;;
;;;; However, execution time depends on several influences:  First, the
;;;; machine you are running the benchmark on.  Second, the compiler you use.
;;;; Third, which compiler options you use.  Fourth, which version of guile
;;;; you are using.  Fifth, which guile options you are using (for example if
;;;; you are using the debugging evaluator or not).  There are even more
;;;; influences.
;;;;
;;;; For this reason, the same number of iterations for a single benchmark may
;;;; lead to completely different execution times in different
;;;; constellations.  For someone working on a slow machine, the default
;;;; execution counts may lead to an inacceptable execution time of the
;;;; benchmark suite.  For someone on a very fast machine, however, it may be
;;;; desireable to increase the number of iterations in order to increase the
;;;; accuracy of the time data.
;;;;
;;;; For this reason, the benchmark suite allows to scale the number of
;;;; executions by a global factor, stored in the exported variable
;;;; iteration-factor.  The default for iteration-factor is 1.  A number of 2
;;;; means, that all benchmarks are executed twice as often, which will also
;;;; roughly double the execution time for the benchmark suite.  Similarly, if
;;;; iteration-factor holds a value of 0.5, only about half the execution time
;;;; will be required.
;;;;
;;;; It is probably a good idea to choose the iteration count for each
;;;; benchmark such that all benchmarks will take about the same time, for
;;;; example one second.  To achieve this, the benchmark suite holds an empty
;;;; benchmark in the file 0-reference.bm named "reference benchmark for
;;;; iteration counts".  It's iteration count is calibrated to make the
;;;; benchmark run about one second on Dirk's laptop :-)  If you are adding
;;;; benchmarks to the suite, it would be nice if you could calibrate the
;;;; number of iterations such that each of your added benchmarks takes about
;;;; as long to run as the reference benchmark.  But:  Don't be too accurate
;;;; to figure out the correct iteration count.


;;;; REPORTERS
;;;;
;;;; A reporter is a function which we apply to each benchmark outcome.
;;;; Reporters can log results, print interesting results to the standard
;;;; output, collect statistics, etc.
;;;;
;;;; A reporter function takes the following arguments:  NAME ITERATIONS
;;;; BEFORE AFTER GC-TIME.  The argument NAME holds the name of the benchmark,
;;;; ITERATIONS holds the actual number of iterations that were performed.
;;;; BEFORE holds the result of the function (times) at the very beginning of
;;;; the excution of the benchmark, AFTER holds the result of the function
;;;; (times) after the execution of the benchmark.  GC-TIME, finally, holds
;;;; the difference of calls to (gc-run-time) before and after the execution
;;;; of the benchmark.
;;;;
;;;; This library provides some standard reporters for logging results
;;;; to a file, reporting interesting results to the user, (FIXME: and
;;;; collecting totals).
;;;;
;;;; You can use the REGISTER-REPORTER function and friends to add whatever
;;;; reporting functions you like.  See under ;;;; TIMING DATA to see how the
;;;; library helps you to extract relevant timing information from the values
;;;; ITERATIONS, BEFORE, AFTER and GC-TIME.  If you don't register any
;;;; reporters, the library uses USER-REPORTER, which writes the most
;;;; interesting results to the standard output.


;;;; TIME CALCULATION
;;;;
;;;; The library uses the guile functions (times) and (gc-run-time) to
;;;; determine the execution time for a single benchmark.  Based on these
;;;; functions, the values of BEFORE, AFTER and GC-TIME are computed, which
;;;; are then passed to the reporter functions.  All three values BEFORE,
;;;; AFTER and GC-TIME include the time needed to executed the benchmark code
;;;; itself, but also the surrounding code that implements the loop to run the
;;;; benchmark code for the given number of times.  This is undesirable, since
;;;; one would prefer to only get the timing data for the benchmarking code.
;;;;
;;;; To cope with this, the benchmarking framework uses a trick:  During
;;;; initialization of the library, the time for executing an empty benchmark
;;;; is measured and stored.  This is an estimate for the time needed by the
;;;; benchmarking framework itself.  For later benchmarks, this time can then
;;;; be subtracted from the measured execution times.
;;;;
;;;; In order to simplify the time calculation for users who want to write
;;;; their own reporters, benchmarking framework provides the following
;;;; definitions:
;;;;
;;;; benchmark-time-base : This variable holds the number of time units that
;;;;     make up a second.  By deviding the results of each of the functions
;;;;     below by this value, you get the corresponding time in seconds.  For
;;;;     example (/ (benchmark-total-time before after) benchmark-time-base)
;;;;     will give you the total time in seconds.
;;;; benchmark-total-time : this function takes two arguments BEFORE and AFTER
;;;;     and computes the total time between the two timestamps.  The result
;;;;     of this function is what the time command of the unix command line
;;;;     would report as real time.
;;;; benchmark-user-time : this function takes two arguments BEFORE and AFTER
;;;;     and computes the time spent in the benchmarking process between the
;;;;     two timestamps.  That means, the time consumed by other processes
;;;;     running on the same machine is not part of the resulting time,
;;;;     neither is time spent within the operating system.  The result of
;;;;     this function is what the time command of the unix command line would
;;;;     report as user time.
;;;; benchmark-system-time : similar to benchmark-user-time, but here the time
;;;;     spent within the operating system is given.  The result of this
;;;;     function is what the time command of the unix command line would
;;;;     report as system time.
;;;; benchmark-frame-time : this function takes the argument ITERATIONS.  It
;;;;     reports the part of the user time that is consumed by the
;;;;     benchmarking framework itself to run some benchmark for the given
;;;;     number of iterations.  You can think of this as the time that would
;;;;     still be consumed, even if the benchmarking code itself was empty.
;;;;     This value does not include any time for garbage collection, even if
;;;;     it is the benchmarking framework which is responsible for causing a
;;;;     garbage collection.
;;;; benchmark-core-time : this function takes three arguments ITERATIONS,
;;;;     BEFORE and AFTER.  It reports the part of the user time that is
;;;;     actually spent within the benchmarking code.  That is, the time
;;;;     needed for the benchmarking framework is subtracted from the user
;;;;     time.  This value, however, includes all garbage collection times,
;;;;     even if some part of the gc-time had actually to be attributed to the
;;;;     benchmarking framework.
;;;; benchmark-user-time\interpreter : this function takes three arguments
;;;;     BEFORE AFTER and GC-TIME.  It reports the part of the user time that
;;;;     is spent in the interpreter (and not in garbage collection).
;;;; benchmark-core-time\interpreter : this function takes four arguments
;;;;     ITERATIONS, BEFORE, AFTER.   and GC-TIME.  It reports the part of the
;;;;     benchmark-core-time that is spent in the interpreter (and not in
;;;;     garbage collection).  This value is most probably the one you are
;;;;     interested in, except if you are doing some garbage collection
;;;;     checks.
;;;; 
;;;; There is no function to calculate the garbage-collection time, since the
;;;; garbage collection time is already passed as an argument GC-TIME to the
;;;; reporter functions.


;;;; MISCELLANEOUS
;;;;

;;; Perform a division and convert the result to inexact.
(define (i/ a b)
  (exact->inexact (/ a b)))

;;; Scale the number of iterations according to the given scaling factor.
(define iteration-factor 1)
(define (scale-iterations iterations)
  (let* ((i (inexact->exact (round (* iterations iteration-factor)))))
    (if (< i 1) 1 i)))


;;;; CORE FUNCTIONS
;;;;

;;; The central routine for executing benchmarks.
;;; The idea is taken from Greg, the GNUstep regression test environment.
(define run-benchmark #f)
(let ((benchmark-running #f))
  (define (local-run-benchmark name iterations thunk)
    (if benchmark-running
	(error "Nested calls to run-benchmark are not permitted.")
	(let ((benchmark-name (full-name name))
	      (iterations (scale-iterations iterations)))
	  (set! benchmark-running #t)
	  (let ((before #f) (after #f) (gc-time #f))
	    (gc)
	    (set! gc-time (gc-run-time))
	    (set! before (times))
	    (do ((i 0 (+ i 1)))
		((= i iterations))
	      (thunk))
	    (set! after (times))
	    (set! gc-time (- (gc-run-time) gc-time))
	    (report benchmark-name iterations before after gc-time))
	  (set! benchmark-running #f))))
  (set! run-benchmark local-run-benchmark))

;;; A short form for benchmarks.
(defmacro benchmark (name iterations body . rest)
  `(,run-benchmark ,name ,iterations (lambda () ,body ,@rest)))


;;;; BENCHMARK NAMES
;;;;

;;;; Turn a benchmark name into a nice human-readable string.
(define (format-benchmark-name name)
  (call-with-output-string
   (lambda (port)
     (let loop ((name name)
		(separator ""))
       (if (pair? name)
	   (begin
	     (display separator port)
	     (display (car name) port)
	     (loop (cdr name) ": ")))))))

;;;; For a given benchmark-name, deliver the full name including all prefixes.
(define (full-name name)
  (append (current-benchmark-prefix) (list name)))

;;; A fluid containing the current benchmark prefix, as a list.
(define prefix-fluid (make-fluid))
(fluid-set! prefix-fluid '())
(define (current-benchmark-prefix)
  (fluid-ref prefix-fluid))

;;; Postpend PREFIX to the current name prefix while evaluting THUNK.
;;; The name prefix is only changed within the dynamic scope of the
;;; call to with-benchmark-prefix*.  Return the value returned by THUNK.
(define (with-benchmark-prefix* prefix thunk)
  (with-fluids ((prefix-fluid
		 (append (fluid-ref prefix-fluid) (list prefix))))
    (thunk)))

;;; (with-benchmark-prefix PREFIX BODY ...)
;;; Postpend PREFIX to the current name prefix while evaluating BODY ...
;;; The name prefix is only changed within the dynamic scope of the
;;; with-benchmark-prefix expression.  Return the value returned by the last
;;; BODY expression.
(defmacro with-benchmark-prefix (prefix . body)
  `(with-benchmark-prefix* ,prefix (lambda () ,@body)))


;;;; TIME CALCULATION
;;;;

(define benchmark-time-base
  internal-time-units-per-second)

(define time-base ;; short-cut, not exported
  benchmark-time-base)

(define frame-time/iteration
  "<will be set during initialization>")

(define (benchmark-total-time before after)
  (- (tms:clock after) (tms:clock before)))

(define (benchmark-user-time before after)
  (- (tms:utime after) (tms:utime before)))

(define (benchmark-system-time before after)
  (- (tms:stime after) (tms:stime before)))

(define (benchmark-frame-time iterations)
  (* iterations frame-time/iteration))

(define (benchmark-core-time iterations before after)
  (- (benchmark-user-time before after) (benchmark-frame-time iterations)))

(define (benchmark-user-time\interpreter before after gc-time)
  (- (benchmark-user-time before after) gc-time))

(define (benchmark-core-time\interpreter iterations before after gc-time)
  (- (benchmark-core-time iterations before after) gc-time))


;;;; REPORTERS
;;;;

;;; The global list of reporters.
(define reporters '())

;;; The default reporter, to be used only if no others exist.
(define default-reporter #f)

;;; Add the procedure REPORTER to the current set of reporter functions.
;;; Signal an error if that reporter procedure object is already registered.
(define (register-reporter reporter)
  (if (memq reporter reporters)
      (error "register-reporter: reporter already registered: " reporter))
  (set! reporters (cons reporter reporters)))

;;; Remove the procedure REPORTER from the current set of reporter
;;; functions.  Signal an error if REPORTER is not currently registered.
(define (unregister-reporter reporter)
  (if (memq reporter reporters)
      (set! reporters (delq! reporter reporters))
      (error "unregister-reporter: reporter not registered: " reporter)))

;;; Return true iff REPORTER is in the current set of reporter functions.
(define (reporter-registered? reporter)
  (if (memq reporter reporters) #t #f))

;;; Send RESULT to all currently registered reporter functions.
(define (report . args)
  (if (pair? reporters)
      (for-each (lambda (reporter) (apply reporter args))
		reporters)
      (apply default-reporter args)))


;;;; Some useful standard reporters:
;;;; Log reporters write all benchmark results to a given log file.
;;;; Full reporters write all benchmark results to the standard output.
;;;; User reporters write some interesting results to the standard output.

;;; Display a single benchmark result to the given port
(define (print-result port name iterations before after gc-time)
  (let* ((name (format-benchmark-name name))
	 (total-time (benchmark-total-time before after))
	 (user-time (benchmark-user-time before after))
	 (system-time (benchmark-system-time before after))
	 (frame-time (benchmark-frame-time iterations))
	 (benchmark-time (benchmark-core-time iterations before after))
	 (user-time\interpreter
	  (benchmark-user-time\interpreter before after gc-time))
	 (benchmark-core-time\interpreter 
	  (benchmark-core-time\interpreter iterations before after gc-time)))
    (write (list name iterations
		 'total (i/ total-time time-base)
		 'user (i/ user-time time-base)
		 'system (i/ system-time time-base)
		 'frame (i/ frame-time time-base)
		 'benchmark (i/ benchmark-time time-base)
		 'user/interp (i/ user-time\interpreter time-base)
		 'bench/interp (i/ benchmark-core-time\interpreter time-base)
		 'gc (i/ gc-time time-base))
	   port)
    (newline port)))

;;; Return a reporter procedure which prints all results to the file
;;; FILE, in human-readable form.  FILE may be a filename, or a port.
(define (make-log-reporter file)
  (let ((port (if (output-port? file) file
		  (open-output-file file))))
    (lambda args
      (apply print-result port args)
      (force-output port))))

;;; A reporter that reports all results to the user.
(define (full-reporter . args)
  (apply print-result (current-output-port) args))

;;; Display interesting results of a single benchmark to the given port
(define (print-user-result port name iterations before after gc-time)
  (let* ((name (format-benchmark-name name))
	 (user-time (benchmark-user-time before after))
	 (benchmark-time (benchmark-core-time iterations before after))
	 (benchmark-core-time\interpreter
	  (benchmark-core-time\interpreter iterations before after gc-time)))
    (write (list name iterations 
		 'user (i/ user-time time-base)
		 'benchmark (i/ benchmark-time time-base)
		 'bench/interp (i/ benchmark-core-time\interpreter time-base)
		 'gc (i/ gc-time time-base))
	   port)
    (newline port)))

;;; A reporter that reports interesting results to the user.
(define (user-reporter . args)
  (apply print-user-result (current-output-port) args))


;;;; Initialize the benchmarking system:
;;;;

;;; First, display version information
(display ";; running guile version " (current-output-port))
(display (version) (current-output-port))
(newline (current-output-port))

;;; Second, make sure the benchmarking routines are compiled.
(define (null-reporter . args) #t)
(set! default-reporter null-reporter)
(benchmark "empty initialization benchmark" 2 #t)

;;; Third, initialize the system constants
(display ";; calibrating the benchmarking framework..." (current-output-port))
(newline (current-output-port))
(define (initialization-reporter name iterations before after gc-time)
  (let* ((frame-time (- (tms:utime after) (tms:utime before) gc-time 3)))
    (set! frame-time/iteration (/ frame-time iterations))
    (display ";; framework time per iteration: " (current-output-port))
    (display (i/ frame-time/iteration time-base) (current-output-port))
    (newline (current-output-port))))
(set! default-reporter initialization-reporter)
(benchmark "empty initialization benchmark" 524288 #t)

;;; Finally, set the default reporter
(set! default-reporter user-reporter)
