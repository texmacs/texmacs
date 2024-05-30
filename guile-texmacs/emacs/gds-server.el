;;; gds-server.el -- infrastructure for running GDS server processes

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


;;;; Customization group setup.

(defgroup gds nil
  "Customization options for Guile Emacs frontend."
  :group 'scheme)


;;;; Communication with the (ice-9 gds-server) subprocess.

;; Subprocess output goes into the `*GDS Process*' buffer, and
;; is then read from there one form at a time.  `gds-read-cursor' is
;; the buffer position of the start of the next unread form.
(defvar gds-read-cursor nil)

;; The guile executable used by the GDS server process.
(defcustom gds-guile-program "guile"
  "*The guile executable used by the GDS server process."
  :type 'string
  :group 'gds)

(defcustom gds-scheme-directory nil
  "Where GDS's Scheme code is, if not in one of the standard places."
  :group 'gds
  :type '(choice (const :tag "nil" nil) directory))

(defun gds-start-server (procname port-or-path protocol-handler &optional bufname)
  "Start a GDS server process called PROCNAME, listening on TCP port
or Unix domain socket PORT-OR-PATH.  PROTOCOL-HANDLER should be a
function that accepts and processes one protocol form.  Optional arg
BUFNAME specifies the name of the buffer that is used for process
output; if not specified the buffer name is the same as the process
name."
  (with-current-buffer (get-buffer-create (or bufname procname))
    (erase-buffer)
    (let* ((code (format "(begin
                            %s
                            (use-modules (ice-9 gds-server))
                            (run-server %S))"
			 (if gds-scheme-directory
			     (concat "(set! %load-path (cons "
				     (format "%S" gds-scheme-directory)
				     " %load-path))")
			   "")
                         port-or-path))
           (process-connection-type nil) ; use a pipe
           (proc (start-process procname
                                (current-buffer)
                                gds-guile-program
                                "-q"
                                "--debug"
                                "-c"
                                code)))
      (set (make-local-variable 'gds-read-cursor) (point-min))
      (set (make-local-variable 'gds-protocol-handler) protocol-handler)
      (set-process-filter proc (function gds-filter))
      (set-process-sentinel proc (function gds-sentinel))
      (set-process-coding-system proc 'latin-1-unix)
      (process-kill-without-query proc)
      proc)))

;; Subprocess output filter: inserts normally into the process buffer,
;; then tries to reread the output one form at a time and delegates
;; processing of each form to `gds-protocol-handler'.
(defun gds-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (insert-before-markers string))
    (goto-char gds-read-cursor)
    (while (let ((form (condition-case nil
			   (read (current-buffer))
			 (error nil))))
	     (if form
		 (save-excursion
		   (funcall gds-protocol-handler (car form) (cdr form))))
	     form)
      (setq gds-read-cursor (point)))))

;; Subprocess sentinel: do nothing.  (Currently just here to avoid
;; inserting un-`read'able process status messages into the process
;; buffer.)
(defun gds-sentinel (proc event)
  )


;;;; The end!

(provide 'gds-server)

;;; gds-server.el ends here.
