;; popen emulation, for non-stdio based ports.

(define-module (ice-9 popen))

;;    (define-module (guile popen)
;;      :use-module (guile posix))

;; a guardian to ensure the cleanup is done correctly when
;; an open pipe is gc'd or a close-port is used.
(define pipe-guardian (make-guardian))

;; a weak hash-table to store the process ids.
(define-public port/pid-table (make-weak-key-hash-table 31))

;; run a process connected to an input or output port.
;; mode: OPEN_READ or OPEN_WRITE.
;; returns port/pid pair.
(define (open-process mode prog . args)
  (let ((p (pipe))
	(reading (string=? mode OPEN_READ)))
    (setvbuf (cdr p) _IONBF)
    (let ((pid (primitive-fork)))
      (cond ((= pid 0)
	     ;; child
	     (set-batch-mode?! #t)
	     (close-all-ports-except (if reading (cdr p) (car p)))
	     (move->fdes (if reading (cdr p) (car p))
			 (if reading 1 0))
	     (apply execlp prog prog args))
	    (else
	     ;; parent
	     (if reading
		 (close-port (cdr p))
		 (close-port (car p)))
	     (cons (if reading
		       (car p)
		       (cdr p))
		   pid))))))

(define-public (open-pipe command mode)
  "Executes the shell command @var{command} (a string) in a subprocess.
A pipe to the process is created and returned.  @var{modes} specifies
whether an input or output pipe to the process is created: it should 
be the value of @code{OPEN_READ} or @code{OPEN_WRITE}."
  (let* ((port/pid (open-process mode "/bin/sh" "-c" command))
	 (port (car port/pid)))
    (pipe-guardian port)
    (hashq-set! port/pid-table port (cdr port/pid))
    port))

(define (fetch-pid port)
  (let ((pid (hashq-ref port/pid-table port)))
    (hashq-remove! port/pid-table port)
    pid))

(define (close-process port/pid)
  (close-port (car port/pid))
  (cdr (waitpid (cdr port/pid))))

;; for the background cleanup handler: just clean up without reporting
;; errors.  also avoids blocking the process: if the child isn't ready
;; to be collected, puts it back into the guardian's live list so it
;; can be tried again the next time the cleanup runs.
(define (close-process-quietly port/pid)
  (catch 'system-error
	 (lambda ()
	   (close-port (car port/pid)))
	 (lambda args #f))
  (catch 'system-error
	 (lambda ()
	   (let ((pid/status (waitpid (cdr port/pid) WNOHANG)))
	     (cond ((= (car pid/status) 0)
		    ;; not ready for collection
		    (pipe-guardian (car port/pid))
		    (hashq-set! port/pid-table
				(car port/pid) (cdr port/pid))))))
	 (lambda args #f)))

(define-public (close-pipe p)
  "Closes the pipe created by @code{open-pipe}, then waits for the process
to terminate and returns its status value, @xref{Processes, waitpid}, for
information on how to interpret this value."
  (let ((pid (fetch-pid p)))
    (if (not pid)
        (error "close-pipe: pipe not in table"))
    (close-process (cons p pid))))

(define reap-pipes
  (lambda ()
    (let loop ((p (pipe-guardian)))
      (cond (p 
	     ;; maybe removed already by close-pipe.
	     (let ((pid (fetch-pid p)))
	       (if pid
		   (close-process-quietly (cons p pid))))
	     (loop (pipe-guardian)))))))

(set! gc-thunk 
      (let ((old-thunk gc-thunk))
	(lambda ()
	  (if old-thunk (old-thunk))
	  (reap-pipes))))

;; (add-hook! after-gc-hook reap-pipes)

(define-public (open-input-pipe command) (open-pipe command OPEN_READ))
(define-public (open-output-pipe command) (open-pipe command OPEN_WRITE))
