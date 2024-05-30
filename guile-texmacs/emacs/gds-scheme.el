;;; gds-scheme.el -- GDS function for Scheme mode buffers

;;;; Copyright (C) 2005 Neil Jerram
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later
;;;; version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;;; 02111-1307 USA

(require 'comint)
(require 'scheme)
(require 'derived)
(require 'pp)

;;;; Maintaining an association between a Guile client process and a
;;;; set of Scheme mode buffers.

(defcustom gds-auto-create-utility-client t
  "Whether to automatically create a utility Guile client, and
associate the current buffer with it, if there are no existing Guile
clients available to GDS when the user does something that requires a
running Guile client."
  :type 'boolean
  :group 'gds)

(defcustom gds-auto-associate-single-client t
  "Whether to automatically associate the current buffer with an
existing Guile client, if there is only only client known to GDS when
the user does something that requires a running Guile client, and the
current buffer is not already associated with a Guile client."
  :type 'boolean
  :group 'gds)

(defcustom gds-auto-associate-last-client t
  "Whether to automatically associate the current buffer with the
Guile client that most recently caused that buffer to be displayed,
when the user does something that requires a running Guile client and
the current buffer is not already associated with a Guile client."
  :type 'boolean
  :group 'gds)

(defvar gds-last-touched-by nil
  "For each Scheme mode buffer, this records the GDS client that most
recently `touched' that buffer in the sense of using it to display
source code, for example for the source code relevant to a debugger
stack frame.")
(make-variable-buffer-local 'gds-last-touched-by)

(defun gds-auto-associate-buffer ()
  "Automatically associate the current buffer with a Guile client, if
possible."
  (let* ((num-clients (length gds-client-info))
	 (client
	  (or
	   ;; If there are no clients yet, and
	   ;; `gds-auto-create-utility-client' allows us to create one
	   ;; automatically, do that.
	   (and (= num-clients 0)
		gds-auto-create-utility-client
		(gds-start-utility-guile))
	   ;; Otherwise, if there is a single existing client, and
	   ;; `gds-auto-associate-single-client' allows us to use it
	   ;; for automatic association, do that.
	   (and (= num-clients 1)
		gds-auto-associate-single-client
		(caar gds-client-info))
	   ;; Otherwise, if the current buffer was displayed because
	   ;; of a Guile client trapping somewhere in its code, and
	   ;; `gds-auto-associate-last-client' allows us to associate
	   ;; with that client, do so.
	   (and gds-auto-associate-last-client
		gds-last-touched-by))))
    (if client
	(gds-associate-buffer client))))	 

(defun gds-associate-buffer (client)
  "Associate the current buffer with the Guile process CLIENT.
This means that operations in this buffer that require a running Guile
process - such as evaluation, help, completion and setting traps -
will be sent to the Guile process whose name or connection number is
CLIENT."
  (interactive (list (gds-choose-client)))
  ;; If this buffer is already associated, dissociate from its
  ;; existing client first.
  (if gds-client (gds-dissociate-buffer))
  ;; Store the client number in the buffer-local variable gds-client.
  (setq gds-client client)
  ;; Add this buffer to the list of buffers associated with the
  ;; client.
  (gds-client-put client 'associated-buffers
		  (cons (current-buffer)
			(gds-client-get client 'associated-buffers))))

(defun gds-dissociate-buffer ()
  "Dissociate the current buffer from any specific Guile process."
  (interactive)
  (if gds-client
      (progn
        ;; Remove this buffer from the list of buffers associated with
        ;; the current client.
	(gds-client-put gds-client 'associated-buffers
			(delq (current-buffer)
			      (gds-client-get gds-client 'associated-buffers)))
        ;; Reset the buffer-local variable gds-client.
        (setq gds-client nil)
        ;; Clear any process status indication from the modeline.
        (setq mode-line-process nil)
        (force-mode-line-update))))

(defun gds-show-client-status (client status-string)
  "Show a client's status in the modeline of all its associated
buffers."
  (let ((buffers (gds-client-get client 'associated-buffers)))
    (while buffers
      (if (buffer-live-p (car buffers))
          (with-current-buffer (car buffers)
            (setq mode-line-process status-string)
            (force-mode-line-update)))
      (setq buffers (cdr buffers)))))

(defcustom gds-running-text ":running"
  "*Mode line text used to show that a Guile process is \"running\".
\"Running\" means that the process cannot currently accept any input
from the GDS frontend in Emacs, because all of its threads are busy
running code that GDS cannot easily interrupt."
  :type 'string
  :group 'gds)

(defcustom gds-ready-text ":ready"
  "*Mode line text used to show that a Guile process is \"ready\".
\"Ready\" means that the process is ready to interact with the GDS
frontend in Emacs, because at least one of its threads is waiting for
GDS input."
  :type 'string
  :group 'gds)

(defcustom gds-debug-text ":debug"
  "*Mode line text used to show that a Guile process is \"debugging\".
\"Debugging\" means that the process is using the GDS frontend in
Emacs to display an error or trap so that the user can debug it."
  :type 'string
  :group 'gds)

(defun gds-choose-client ()
  "Ask the user to choose a GDS client process from a list."
  (let ((table '())
        (default nil))
    ;; Prepare a table containing all current clients.
    (mapcar (lambda (client-info)
               (setq table (cons (cons (cadr (memq 'name client-info))
				       (car client-info))
				 table)))
             gds-client-info)
    ;; Add an entry to allow the user to ask for a new process.
    (setq table (cons (cons "Start a new Guile process" nil) table))
    ;; Work out a good default.  If the buffer has a good value in
    ;; gds-last-touched-by, we use that; otherwise default to starting
    ;; a new process.
    (setq default (or (and gds-last-touched-by
                           (gds-client-get gds-last-touched-by 'name))
                      (caar table)))
    ;; Read using this table.
    (let* ((name (completing-read "Choose a Guile process: "
                                  table
                                  nil
                                  t     ; REQUIRE-MATCH
                                  nil   ; INITIAL-INPUT
                                  nil   ; HIST
                                  default))
           ;; Convert name to a client number.
           (client (cdr (assoc name table))))
      ;; If the user asked to start a new Guile process, do that now.
      (or client (setq client (gds-start-utility-guile)))
      ;; Return the chosen client ID.
      client)))

(defvar gds-last-utility-number 0
  "Number of the last started Guile utility process.")

(defun gds-start-utility-guile ()
  "Start a new utility Guile process."
  (setq gds-last-utility-number (+ gds-last-utility-number 1))
  (let* ((procname (format "gds-util[%d]" gds-last-utility-number))
         (code (format "(begin
                          %s
                          (use-modules (ice-9 gds-client))
                          (run-utility))"
		       (if gds-scheme-directory
			   (concat "(set! %load-path (cons "
				   (format "%S" gds-scheme-directory)
				   " %load-path))")
			 "")))
         (proc (start-process procname
                              (get-buffer-create procname)
                              gds-guile-program
                              "-q"
                              "--debug"
                              "-c"
                              code))
         (client nil))
    ;; Note that this process can be killed automatically on Emacs
    ;; exit.
    (process-kill-without-query proc)
    ;; Set up a process filter to catch the new client's number.
    (set-process-filter proc
                        (lambda (proc string)
                          (setq client (string-to-number string))
                          (if (process-buffer proc)
                              (with-current-buffer (process-buffer proc)
                                (insert string)))))
    ;; Accept output from the new process until we have its number.
    (while (not client)
      (accept-process-output proc))
    ;; Return the new process's client number.
    client))

;;;; Evaluating code.

;; The following commands send code for evaluation through the GDS TCP
;; connection, receive the result and any output generated through the
;; same connection, and display the result and output to the user.
;;
;; For each buffer where evaluations can be requested, GDS uses the
;; buffer-local variable `gds-client' to track which GDS client
;; program should receive and handle that buffer's evaluations.

(defun gds-module-name (start end)
  "Determine and return the name of the module that governs the
specified region.  The module name is returned as a list of symbols."
  (interactive "r")			; why not?
  (save-excursion
    (goto-char start)
    (let (module-name)
      (while (and (not module-name)
		  (beginning-of-defun-raw 1))
	(if (looking-at "(define-module ")
	    (setq module-name
		  (progn
		    (goto-char (match-end 0))
		    (read (current-buffer))))))
      module-name)))

(defcustom gds-emacs-buffer-port-name-prefix "Emacs buffer: "
  "Prefix used when telling Guile the name of the port from which a
chunk of Scheme code (to be evaluated) comes.  GDS uses this prefix,
followed by the buffer name, in two cases: when the buffer concerned
is not associated with a file, or if the buffer has been modified
since last saving to its file.  In the case where the buffer is
identical to a saved file, GDS uses the file name as the port name."
  :type '(string)
  :group 'gds)

(defun gds-port-name (start end)
  "Return port name for the specified region of the current buffer.
The name will be used by Guile as the port name when evaluating that
region's code."
  (or (and (not (buffer-modified-p))
	   buffer-file-name)
      (concat gds-emacs-buffer-port-name-prefix (buffer-name))))

(defun gds-line-and-column (pos)
  "Return 0-based line and column number at POS."
  (let (line column)
    (save-excursion
      (goto-char pos)
      (setq column (current-column))
      (beginning-of-line)
      (setq line (count-lines (point-min) (point))))
    (cons line column)))

(defun gds-eval-region (start end &optional debugp)
  "Evaluate the current region.  If invoked with `C-u' prefix (or, in
a program, with optional DEBUGP arg non-nil), pause and pop up the
stack at the start of the evaluation, so that the user can single-step
through the code."
  (interactive "r\nP")
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (let ((module (gds-module-name start end))
	(port-name (gds-port-name start end))
	(lc (gds-line-and-column start)))
    (let ((code (buffer-substring-no-properties start end)))
      (gds-send (format "eval (region . %S) %s %S %d %d %S %s"
			(gds-abbreviated code)
			(if module (prin1-to-string module) "#f")
			port-name (car lc) (cdr lc)
			code
			(if debugp '(debug) '(none)))
		gds-client))))

(defun gds-eval-expression (expr &optional correlator debugp)
  "Evaluate the supplied EXPR (a string).  If invoked with `C-u'
prefix (or, in a program, with optional DEBUGP arg non-nil), pause and
pop up the stack at the start of the evaluation, so that the user can
single-step through the code."
  (interactive "sEvaluate expression: \ni\nP")
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (set-text-properties 0 (length expr) nil expr)
  (gds-send (format "eval (%S . %S) #f \"Emacs expression\" 0 0 %S %s"
		    (or correlator 'expression)
		    (gds-abbreviated expr)
		    expr
		    (if debugp '(debug) '(none)))
	    gds-client))

(defconst gds-abbreviated-length 35)

(defun gds-abbreviated (code)
  (let ((nlpos (string-match (regexp-quote "\n") code)))
    (while nlpos
      (setq code
	    (if (= nlpos (- (length code) 1))
		(substring code 0 nlpos)
	      (concat (substring code 0 nlpos)
		      "\\n"
		      (substring code (+ nlpos 1)))))
      (setq nlpos (string-match (regexp-quote "\n") code))))
  (if (> (length code) gds-abbreviated-length)
      (concat (substring code 0 (- gds-abbreviated-length 3)) "...")
    code))

(defun gds-eval-defun (&optional debugp)
  "Evaluate the defun (top-level form) at point.  If invoked with
`C-u' prefix (or, in a program, with optional DEBUGP arg non-nil),
pause and pop up the stack at the start of the evaluation, so that the
user can single-step through the code."
  (interactive "P")
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (gds-eval-region (point) end debugp))))

(defun gds-eval-last-sexp (&optional debugp)
  "Evaluate the sexp before point.  If invoked with `C-u' prefix (or,
in a program, with optional DEBUGP arg non-nil), pause and pop up the
stack at the start of the evaluation, so that the user can single-step
through the code."
  (interactive "P")
  (gds-eval-region (save-excursion (backward-sexp) (point)) (point) debugp))

;;;; Help.

;; Help is implemented as a special case of evaluation, identified by
;; the evaluation correlator 'help.

(defun gds-help-symbol (sym)
  "Get help for SYM (a Scheme symbol)."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Describe Guile symbol (default %s): " sym)
		  "Describe Guile symbol: ")))
     (list (if (zerop (length val)) sym val))))
  (gds-eval-expression (format "(help %s)" sym) 'help))

(defun gds-apropos (regex)
  "List Guile symbols matching REGEX."
  (interactive
   (let ((sym (thing-at-point 'symbol))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-from-minibuffer
		(if sym
		    (format "Guile apropos (regexp, default \"%s\"): " sym)
		  "Guile apropos (regexp): ")))
     (list (if (zerop (length val)) sym val))))
  (set-text-properties 0 (length regex) nil regex)
  (gds-eval-expression (format "(apropos %S)" regex) 'apropos))

;;;; Displaying results of help and eval.

(defun gds-display-results (client correlator stack-available results)
  (let* ((helpp+bufname (cond ((eq (car correlator) 'help)
                               '(t . "*Guile Help*"))
                              ((eq (car correlator) 'apropos)
                               '(t . "*Guile Apropos*"))
                              (t
                               '(nil . "*Guile Evaluation*"))))
         (helpp (car helpp+bufname)))
    (let ((buf (get-buffer-create (cdr helpp+bufname))))
      (save-selected-window
	(save-excursion
	  (set-buffer buf)
	  (gds-dissociate-buffer)
	  (erase-buffer)
	  (scheme-mode)
	  (insert (cdr correlator) "\n\n")
	  (while results
	    (insert (car results))
	    (or (bolp) (insert "\\\n"))
	    (if helpp
		nil
	      (if (cadr results)
		  (mapcar (function (lambda (value)
				      (insert " => " value "\n")))
			  (cadr results))
		(insert " => no (or unspecified) value\n"))
	      (insert "\n"))
	    (setq results (cddr results)))
	  (if stack-available
	      (let ((beg (point))
		    (map (make-sparse-keymap)))
		(define-key map [mouse-1] 'gds-show-last-stack)
		(define-key map "\C-m" 'gds-show-last-stack)
		(insert "[click here to show error stack]")
		(add-text-properties beg (point)
				     (list 'keymap map
					   'mouse-face 'highlight))
		(insert "\n")))
	  (goto-char (point-min))
	  (gds-associate-buffer client))
	(pop-to-buffer buf)
	(run-hooks 'temp-buffer-show-hook)))))

(defun gds-show-last-stack ()
  "Show stack of the most recent error."
  (interactive)
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (gds-send "debug-lazy-trap-context" gds-client))

;;;; Completion.

(defvar gds-completion-results nil)

(defun gds-complete-symbol ()
  "Complete the Guile symbol before point.  Returns `t' if anything
interesting happened, `nil' if not."
  (interactive)
  (or gds-client
      (gds-auto-associate-buffer)
      (call-interactively 'gds-associate-buffer))
  (let* ((chars (- (point) (save-excursion
			     (while (let ((syntax (char-syntax (char-before (point)))))
				      (or (eq syntax ?w) (eq syntax ?_)))
			       (forward-char -1))
			     (point)))))
    (if (zerop chars)
	nil
      (setq gds-completion-results nil)
      (gds-send (format "complete %s"
			(prin1-to-string
			 (buffer-substring-no-properties (- (point) chars)
							 (point))))
		 gds-client)
      (while (null gds-completion-results)
	(accept-process-output gds-debug-server 0 200))
      (cond ((eq gds-completion-results 'error)
             (error "Internal error - please report the contents of the *Guile Evaluation* window"))
	    ((eq gds-completion-results t)
	     nil)
	    ((stringp gds-completion-results)
	     (if (<= (length gds-completion-results) chars)
		 nil
	       (insert (substring gds-completion-results chars))
	       (message "Sole completion")
	       t))
	    ((= (length gds-completion-results) 1)
	     (if (<= (length (car gds-completion-results)) chars)
		 nil
	       (insert (substring (car gds-completion-results) chars))
	       t))
	    (t
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list gds-completion-results))
	     t)))))

;;;; Dispatcher for non-debug protocol.

(defun gds-nondebug-protocol (client proc args)
  (cond (;; (eval-results ...) - Results of evaluation.
         (eq proc 'eval-results)
         (gds-display-results client (car args) (cadr args) (cddr args))
         ;; If these results indicate an error, set
         ;; gds-completion-results to non-nil in case the error arose
         ;; when trying to do a completion.
         (if (eq (caar args) 'error)
             (setq gds-completion-results 'error)))

        (;; (completion-result ...) - Available completions.
         (eq proc 'completion-result)
         (setq gds-completion-results (or (car args) t)))

	(;; (note ...) - For debugging only.
	 (eq proc 'note))

        (;; (trace ...) - Tracing.
         (eq proc 'trace)
         (with-current-buffer (get-buffer-create "*GDS Trace*")
           (save-excursion
             (goto-char (point-max))
             (or (bolp) (insert "\n"))
             (insert "[client " (number-to-string client) "] " (car args) "\n"))))

        (t
         ;; Unexpected.
         (error "Bad protocol: %S" form))))
  
;;;; Scheme mode keymap items.

(define-key scheme-mode-map "\M-\C-x" 'gds-eval-defun)
(define-key scheme-mode-map "\C-x\C-e" 'gds-eval-last-sexp)
(define-key scheme-mode-map "\C-c\C-e" 'gds-eval-expression)
(define-key scheme-mode-map "\C-c\C-r" 'gds-eval-region)
(define-key scheme-mode-map "\C-hg" 'gds-help-symbol)
(define-key scheme-mode-map "\C-h\C-g" 'gds-apropos)
(define-key scheme-mode-map "\C-hG" 'gds-apropos)
(define-key scheme-mode-map "\C-hS" 'gds-show-last-stack)
(define-key scheme-mode-map "\e\t" 'gds-complete-symbol)

;;;; The end!

(provide 'gds-scheme)

;;; gds-scheme.el ends here.
