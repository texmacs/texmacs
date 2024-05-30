;;; gds.el -- frontend for Guile development in Emacs

;;;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

; TODO:
; ?transcript
; scheme-mode menu
; interrupt/sigint/async-break
; (module browsing)
; load file
; doing common protocol from debugger
; thread override for debugging

;;;; Prerequisites.

(require 'scheme)
(require 'cl)
(require 'gds-server)
(require 'gds-scheme)

;; The subprocess object for the debug server.
(defvar gds-debug-server nil)

(defvar gds-socket-type-alist '((tcp . 8333)
				(unix . "/tmp/.gds_socket"))
  "Maps each of the possible socket types that the GDS server can
listen on to the path that it should bind to for each one.")

(defun gds-run-debug-server ()
  "Start (or restart, if already running) the GDS debug server process."
  (interactive)
  (if gds-debug-server (gds-kill-debug-server))
  (setq gds-debug-server
        (gds-start-server "gds-debug"
			  (cdr (assq gds-server-socket-type
				     gds-socket-type-alist))
			  'gds-debug-protocol))
  (process-kill-without-query gds-debug-server))

(defun gds-kill-debug-server ()
  "Kill the GDS debug server process."
  (interactive)
  (mapcar (function gds-client-gone)
	  (mapcar (function car) gds-client-info))
  (condition-case nil
      (progn
	(kill-process gds-debug-server)
	(accept-process-output gds-debug-server 0 200))
    (error))
  (setq gds-debug-server nil))

;; Send input to the subprocess.
(defun gds-send (string client)
  (with-current-buffer (get-buffer-create "*GDS Transcript*")
    (goto-char (point-max))
    (insert (number-to-string client) ": (" string ")\n"))
  (gds-client-put client 'thread-id nil)
  (gds-show-client-status client gds-running-text)
  (process-send-string gds-debug-server (format "(%S %s)\n" client string)))


;;;; Per-client information

(defun gds-client-put (client property value)
  (let ((client-info (assq client gds-client-info)))
    (if client-info
	(let ((prop-info (memq property client-info)))
	  (if prop-info
	      (setcar (cdr prop-info) value)
	    (setcdr client-info
		    (list* property value (cdr client-info)))))
      (setq gds-client-info
	    (cons (list client property value) gds-client-info)))))

(defun gds-client-get (client property)
  (let ((client-info (assq client gds-client-info)))
    (and client-info
	 (cadr (memq property client-info)))))

(defvar gds-client-info '())

(defun gds-get-client-buffer (client)
  (let ((existing-buffer (gds-client-get client 'stack-buffer)))
    (if (and existing-buffer
	     (buffer-live-p existing-buffer))
	existing-buffer
      (let ((new-buffer (generate-new-buffer (gds-client-get client 'name))))
	(with-current-buffer new-buffer
	  (gds-debug-mode)
	  (setq gds-client client)
	  (setq gds-stack nil))
	(gds-client-put client 'stack-buffer new-buffer)
	new-buffer))))

(defun gds-client-gone (client &rest ignored)
  ;; Kill the client's stack buffer, if it has one.
  (let ((stack-buffer (gds-client-get client 'stack-buffer)))
    (if (and stack-buffer
	     (buffer-live-p stack-buffer))
	(kill-buffer stack-buffer)))
  ;; Dissociate all the client's associated buffers.
  (mapcar (function (lambda (buffer)
		      (if (buffer-live-p buffer)
			  (with-current-buffer buffer
			    (gds-dissociate-buffer)))))
	  (copy-sequence (gds-client-get client 'associated-buffers)))
  ;; Remove this client's record from gds-client-info.
  (setq gds-client-info (delq (assq client gds-client-info) gds-client-info)))

(defvar gds-client nil)
(make-variable-buffer-local 'gds-client)

(defvar gds-stack nil)
(make-variable-buffer-local 'gds-stack)

(defvar gds-tweaking nil)
(make-variable-buffer-local 'gds-tweaking)

(defvar gds-selected-frame-index nil)
(make-variable-buffer-local 'gds-selected-frame-index)


;;;; Debugger protocol

(defun gds-debug-protocol (client form)
  (or (eq client '*)
      (let ((proc (car form)))
        (cond ((eq proc 'name)
               ;; (name ...) - client name.
	       (gds-client-put client 'name (caddr form)))

              ((eq proc 'stack)
               ;; (stack ...) - stack information.
               (with-current-buffer (gds-get-client-buffer client)
                 (setq gds-stack (cddr form))
                 (setq gds-tweaking (memq 'instead (cadr gds-stack)))
                 (setq gds-selected-frame-index (cadr form))
                 (gds-display-stack)))

              ((eq proc 'closed)
               ;; (closed) - client has gone/died.
               (gds-client-gone client))

              ((eq proc 'eval-result)
               ;; (eval-result RESULT) - result of evaluation.
	       (if gds-last-eval-result
		   (message "%s" (cadr form))
		 (setq gds-last-eval-result (cadr form))))

              ((eq proc 'info-result)
               ;; (info-result RESULT) - info about selected frame.
               (message "%s" (cadr form)))

	      ((eq proc 'thread-id)
               ;; (thread-id THREAD) - says which client thread is reading.
               (let ((thread-id (cadr form))
                     (debug-thread-id (gds-client-get client 'debug-thread-id)))
                 (if (and debug-thread-id
                          (/= thread-id debug-thread-id))
                     ;; Tell the newly reading thread to go away.
                     (gds-send "dismiss" client)
                   ;; Either there's no current debug-thread-id, or
                   ;; the thread now reading is the debug thread.
                   (if debug-thread-id
                       (progn
                         ;; Reset the debug-thread-id.
                         (gds-client-put client 'debug-thread-id nil)
                         ;; Indicate debug status in modelines.
                         (gds-show-client-status client gds-debug-text))
                     ;; Indicate normal read status in modelines..
                     (gds-show-client-status client gds-ready-text)))))

	      ((eq proc 'debug-thread-id)
               ;; (debug-thread-id THREAD) - debug override indication.
               (gds-client-put client 'debug-thread-id (cadr form))
               ;; If another thread is already reading, send it away.
               (if (gds-client-get client 'thread-id)
                   (gds-send "dismiss" client)))

              (t
               ;; Non-debug-specific protocol.
               (gds-nondebug-protocol client proc (cdr form)))))))


;;;; Displaying a stack

(define-derived-mode gds-debug-mode
  scheme-mode
  "Guile-Debug"
  "Major mode for debugging a Guile client application."
  (use-local-map gds-mode-map))

(defun gds-display-stack-first-line ()
  (let ((flags (cadr gds-stack)))
    (cond ((memq 'application flags)
           (insert "Calling procedure:\n"))
          ((memq 'evaluation flags)
           (insert "Evaluating expression"
                   (cond ((stringp gds-tweaking) (format " (tweaked: %s)"
							 gds-tweaking))
			 (gds-tweaking " (tweakable)")
			 (t ""))
                   ":\n"))
          ((memq 'return flags)
           (let ((value (cadr (memq 'return flags))))
             (while (string-match "\n" value)
               (setq value (replace-match "\\n" nil t value)))
             (insert "Return value"
                     (cond ((stringp gds-tweaking) (format " (tweaked: %s)"
							   gds-tweaking))
			   (gds-tweaking " (tweakable)")
			   (t ""))
                     ": " value "\n")))
          ((memq 'error flags)
           (let ((value (cadr (memq 'error flags))))
             (while (string-match "\n" value)
               (setq value (replace-match "\\n" nil t value)))
             (insert "Error: " value "\n")))
          (t
           (insert "Stack: " (prin1-to-string flags) "\n")))))

(defun gds-display-stack ()
  (if gds-undisplay-timer
      (cancel-timer gds-undisplay-timer))
  (setq gds-undisplay-timer nil)
  ;(setq buffer-read-only nil)
  (mapcar 'delete-overlay
          (overlays-in (point-min) (point-max)))
  (erase-buffer)
  (gds-display-stack-first-line)
  (let ((frames (car gds-stack)))
    (while frames
      (let ((frame-text (cadr (car frames)))
            (frame-source (caddr (car frames))))
        (while (string-match "\n" frame-text)
          (setq frame-text (replace-match "\\n" nil t frame-text)))
        (insert "   "
                (if frame-source "s" " ")
                frame-text
                "\n"))
      (setq frames (cdr frames))))
  ;(setq buffer-read-only t)
  (gds-show-selected-frame))

(defun gds-tweak (expr)
  (interactive "sTweak expression or return value: ")
  (or gds-tweaking
      (error "The current stack cannot be tweaked"))
  (setq gds-tweaking
        (if (> (length expr) 0)
            expr
          t))
  (save-excursion
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    (gds-display-stack-first-line)))

(defvar gds-undisplay-timer nil)
(make-variable-buffer-local 'gds-undisplay-timer)

(defvar gds-undisplay-wait 1)

(defun gds-undisplay-buffer ()
  (if gds-undisplay-timer
      (cancel-timer gds-undisplay-timer))
  (setq gds-undisplay-timer
        (run-at-time gds-undisplay-wait
                     nil
                     (function kill-buffer)
                     (current-buffer))))
                                 
(defun gds-show-selected-frame ()
  (setq gds-local-var-cache nil)
  (goto-char (point-min))
  (forward-line (+ gds-selected-frame-index 1))
  (delete-char 3)
  (insert "=> ")
  (beginning-of-line)
  (gds-show-selected-frame-source (caddr (nth gds-selected-frame-index
                                              (car gds-stack)))))

(defun gds-unshow-selected-frame ()
  (if gds-frame-source-overlay
      (move-overlay gds-frame-source-overlay 0 0))
  (save-excursion
    (goto-char (point-min))
    (forward-line (+ gds-selected-frame-index 1))
    (delete-char 3)
    (insert "   ")))

;; Overlay used to highlight the source expression corresponding to
;; the selected frame.
(defvar gds-frame-source-overlay nil)

(defcustom gds-source-file-name-transforms nil
  "Alist of regexps and substitutions for transforming Scheme source
file names.  Each element in the alist is (REGEXP . SUBSTITUTION).
Each source file name in a Guile backtrace is compared against each
REGEXP in turn until the first one that matches, then `replace-match'
is called with SUBSTITUTION to transform that file name.

This mechanism targets the situation where you are working on a Guile
application and want to install it, in /usr/local say, before each
test run.  In this situation, even though Guile is reading your Scheme
files from /usr/local/share/guile, you probably want Emacs to pop up
the corresponding files from your working codebase instead.  Therefore
you would add an element to this alist to transform
\"^/usr/local/share/guile/whatever\" to \"~/codebase/whatever\"."
  :type '(alist :key-type regexp :value-type string)
  :group 'gds)

(defun gds-show-selected-frame-source (source)
  ;; Highlight the frame source, if possible.
  (if source
      (let ((filename (car source))
            (client gds-client)
	    (transforms gds-source-file-name-transforms))
	;; Apply possible transforms to the source file name.
	(while transforms
	  (if (string-match (caar transforms) filename)
	      (let ((trans-fn (replace-match (cdar transforms)
					     t nil filename)))
		(if (file-readable-p trans-fn)
		    (setq filename trans-fn
			  transforms nil))))
	  (setq transforms (cdr transforms)))
	;; Try to map the (possibly transformed) source file to a
	;; buffer.
	(let ((source-buffer (gds-source-file-name-to-buffer filename)))
	  (if source-buffer
	      (with-current-buffer source-buffer
		(if gds-frame-source-overlay
		    nil
		  (setq gds-frame-source-overlay (make-overlay 0 0))
		  (overlay-put gds-frame-source-overlay 'face 'highlight)
                  (overlay-put gds-frame-source-overlay
                               'help-echo
                               (function gds-show-local-var)))
		;; Move to source line.  Note that Guile line numbering
		;; is 0-based, while Emacs numbering is 1-based.
		(save-restriction
		  (widen)
		  (goto-line (+ (cadr source) 1))
		  (move-to-column (caddr source))
		  (move-overlay gds-frame-source-overlay
				(point)
				(if (not (looking-at ")"))
				    (save-excursion (forward-sexp 1) (point))
				  ;; It seems that the source
				  ;; coordinates for backquoted
				  ;; expressions are at the end of the
				  ;; sexp rather than the beginning...
				  (save-excursion (forward-char 1)
						  (backward-sexp 1) (point)))
				(current-buffer)))
		;; Record that this source buffer has been touched by a
		;; GDS client process.
		(setq gds-last-touched-by client))
	    (message "Source for this frame cannot be shown: %s:%d:%d"
		     filename
		     (cadr source)
		     (caddr source)))))
    (message "Source for this frame was not recorded"))
  (gds-display-buffers))

(defvar gds-local-var-cache nil)

(defun gds-show-local-var (window overlay position)
  (let ((frame-index gds-selected-frame-index)
	(client gds-client))
    (with-current-buffer (overlay-buffer overlay)
      (save-excursion
        (goto-char position)
        (let ((gds-selected-frame-index frame-index)
	      (gds-client client)
	      (varname (thing-at-point 'symbol))
	      (state (parse-partial-sexp (overlay-start overlay) (point))))
          (when (and gds-selected-frame-index
		     gds-client
		     varname
		     (not (or (nth 3 state)
			      (nth 4 state))))
	    (set-text-properties 0 (length varname) nil varname)
            (let ((existing (assoc varname gds-local-var-cache)))
              (if existing
                  (cdr existing)
                (gds-evaluate varname)
                (setq gds-last-eval-result nil)
                (while (not gds-last-eval-result)
                  (accept-process-output gds-debug-server))
                (setq gds-local-var-cache
                      (cons (cons varname gds-last-eval-result)
                            gds-local-var-cache))
                gds-last-eval-result))))))))

(defun gds-source-file-name-to-buffer (filename)
  ;; See if filename begins with gds-emacs-buffer-port-name-prefix.
  (if (string-match (concat "^"
			    (regexp-quote gds-emacs-buffer-port-name-prefix))
		    filename)
      ;; It does, so get the named buffer.
      (get-buffer (substring filename (match-end 0)))
    ;; It doesn't, so treat as a file name.
    (and (file-readable-p filename)
	 (find-file-noselect filename))))

(defun gds-select-stack-frame (&optional frame-index)
  (interactive)
  (let ((new-frame-index (or frame-index
                             (gds-current-line-frame-index))))
    (or (and (>= new-frame-index 0)
             (< new-frame-index (length (car gds-stack))))
        (error (if frame-index
                   "No more frames in this direction"
                 "No frame here")))
    (gds-unshow-selected-frame)
    (setq gds-selected-frame-index new-frame-index)
    (gds-show-selected-frame)))

(defun gds-up ()
  (interactive)
  (gds-select-stack-frame (- gds-selected-frame-index 1)))

(defun gds-down ()
  (interactive)
  (gds-select-stack-frame (+ gds-selected-frame-index 1)))

(defun gds-current-line-frame-index ()
  (- (count-lines (point-min)
                  (save-excursion
                    (beginning-of-line)
                    (point)))
     1))

(defun gds-display-buffers ()
  (let ((buf (current-buffer)))
    ;; If there's already a window showing the buffer, use it.
    (let ((window (get-buffer-window buf t)))
      (if window
          (progn
            (make-frame-visible (window-frame window))
            (select-window window))
        (switch-to-buffer buf)
        (setq window (get-buffer-window buf t))))
    ;; If there is an associated source buffer, display it as well.
    (if (and gds-frame-source-overlay
	     (overlay-end gds-frame-source-overlay)
	     (> (overlay-end gds-frame-source-overlay) 1))
        (progn
          (delete-other-windows)
          (let ((window (display-buffer
                         (overlay-buffer gds-frame-source-overlay))))
            (set-window-point window
                              (overlay-start gds-frame-source-overlay)))))))


;;;; Debugger commands.

;; Typically but not necessarily used from the `stack' view.

(defun gds-send-tweaking ()
  (if (stringp gds-tweaking)
      (gds-send (format "tweak %S" gds-tweaking) gds-client)))

(defun gds-go ()
  (interactive)
  (gds-send-tweaking)
  (gds-send "continue" gds-client)
  (gds-unshow-selected-frame)
  (gds-undisplay-buffer))

(defvar gds-last-eval-result t)

(defun gds-evaluate (expr)
  (interactive "sEvaluate variable or expression: ")
  (gds-send (format "evaluate %d %s"
                    gds-selected-frame-index
                    (prin1-to-string expr))
	    gds-client))

(defun gds-frame-info ()
  (interactive)
  (gds-send (format "info-frame %d" gds-selected-frame-index)
            gds-client))

(defun gds-frame-args ()
  (interactive)
  (gds-send (format "info-args %d" gds-selected-frame-index)
            gds-client))

(defun gds-proc-source ()
  (interactive)
  (gds-send (format "proc-source %d" gds-selected-frame-index)
            gds-client))

(defun gds-traps-here ()
  (interactive)
  (gds-send "traps-here" gds-client))

(defun gds-step-into ()
  (interactive)
  (gds-send-tweaking)
  (gds-send (format "step-into %d" gds-selected-frame-index)
            gds-client)
  (gds-unshow-selected-frame)
  (gds-undisplay-buffer))

(defun gds-step-over ()
  (interactive)
  (gds-send-tweaking)
  (gds-send (format "step-over %d" gds-selected-frame-index)
            gds-client)
  (gds-unshow-selected-frame)
  (gds-undisplay-buffer))

(defun gds-step-file ()
  (interactive)
  (gds-send-tweaking)
  (gds-send (format "step-file %d" gds-selected-frame-index)
            gds-client)
  (gds-unshow-selected-frame)
  (gds-undisplay-buffer))




;;;; Guile Interaction mode keymap and menu items.

(defvar gds-mode-map (make-sparse-keymap))
(define-key gds-mode-map "c" (function gds-go))
(define-key gds-mode-map "g" (function gds-go))
(define-key gds-mode-map "q" (function gds-go))
(define-key gds-mode-map "e" (function gds-evaluate))
(define-key gds-mode-map "I" (function gds-frame-info))
(define-key gds-mode-map "A" (function gds-frame-args))
(define-key gds-mode-map "S" (function gds-proc-source))
(define-key gds-mode-map "T" (function gds-traps-here))
(define-key gds-mode-map "\C-m" (function gds-select-stack-frame))
(define-key gds-mode-map "u" (function gds-up))
(define-key gds-mode-map [up] (function gds-up))
(define-key gds-mode-map "\C-p" (function gds-up))
(define-key gds-mode-map "d" (function gds-down))
(define-key gds-mode-map [down] (function gds-down))
(define-key gds-mode-map "\C-n" (function gds-down))
(define-key gds-mode-map " " (function gds-step-file))
(define-key gds-mode-map "i" (function gds-step-into))
(define-key gds-mode-map "o" (function gds-step-over))
(define-key gds-mode-map "t" (function gds-tweak))


(defvar gds-menu nil
  "Global menu for GDS commands.")
(if nil;gds-menu
    nil
  (setq gds-menu (make-sparse-keymap "Guile-Debug"))
  (define-key gds-menu [traps-here]
    '(menu-item "Show Traps Here" gds-traps-here))
  (define-key gds-menu [proc-source]
    '(menu-item "Show Procedure Source" gds-proc-source))
  (define-key gds-menu [frame-args]
    '(menu-item "Show Frame Args" gds-frame-args))
  (define-key gds-menu [frame-info]
    '(menu-item "Show Frame Info" gds-frame-info))
  (define-key gds-menu [separator-1]
    '("--"))
  (define-key gds-menu [evaluate]
    '(menu-item "Evaluate..." gds-evaluate))
  (define-key gds-menu [separator-2]
    '("--"))
  (define-key gds-menu [down]
    '(menu-item "Move Down A Frame" gds-down))
  (define-key gds-menu [up]
    '(menu-item "Move Up A Frame" gds-up))
  (define-key gds-menu [separator-3]
    '("--"))
  (define-key gds-menu [step-over]
    '(menu-item "Step Over Current Expression" gds-step-over))
  (define-key gds-menu [step-into]
    '(menu-item "Step Into Current Expression" gds-step-into))
  (define-key gds-menu [step-file]
    '(menu-item "Step Through Current Source File" gds-step-file))
  (define-key gds-menu [separator-4]
    '("--"))
  (define-key gds-menu [go]
    '(menu-item "Go  [continue execution]" gds-go))
  (define-key gds-mode-map [menu-bar gds-debug]
    (cons "Guile-Debug" gds-menu)))


;;;; Autostarting the GDS server.

(defcustom gds-autorun-debug-server t
  "Whether to automatically run the GDS server when `gds.el' is loaded."
  :type 'boolean
  :group 'gds)

(defcustom gds-server-socket-type 'tcp
  "What kind of socket the GDS server should listen on."
  :group 'gds
  :type '(choice (const :tag "TCP" tcp)
		 (const :tag "Unix" unix)))

;;;; If requested, autostart the server after loading.

(if (and gds-autorun-debug-server
	 (not gds-debug-server))
    (gds-run-debug-server))

;;;; The end!

(provide 'gds)

;;; gds.el ends here.
