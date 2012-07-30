
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : files.scm
;; DESCRIPTION : file handling
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-files)
  (:use (texmacs texmacs tm-server)
        (texmacs texmacs tm-print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (propose-name-buffer)
  (with name (url->unix (current-buffer))
    (cond ((not (url-scratch? name)) name)
	  ((os-win32?) "")
	  (else (string-append (var-eval-system "pwd") "/")))))

(tm-property (choose-file fun text type)
  (:interactive #t))

(tm-define (open-auxiliary aux body . opt-master)
  (let* ((name (aux-name aux))
         (master (if (null? opt-master) (buffer-master) (car opt-master))))
    (aux-set-document aux body)
    (aux-set-master aux master)
    (switch-to-buffer name)))

(define-public-macro (with-aux u . prg)
  `(let* ((u ,u)
	  (t (tree-import u "texmacs"))
	  (name (current-buffer)))
     (open-auxiliary "* Aux *" t u)
     (with r (begin ,@prg)
       (switch-to-buffer name)
       r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define current-save-target (url-none))

(define (buffer-notify-recent name)
  (learn-interactive 'recent-buffer (list (cons "0" (url->unix name)))))

(define (has-faithful-format? name)
  (in? (url-suffix name) '("tm" "ts" "tp" "stm" "tmml")))

(define (save-buffer-post name opts)
  ;;(display* "save-buffer-post " name "\n")
  (cond ((in? :update opts)
         (update-buffer name))
        ((in? :commit opts)
         (commit-buffer name))))

(define (save-buffer-save name opts)
  ;;(display* "save-buffer-save " name "\n")
  (with vname `(verbatim ,(url->system name))
    (if (buffer-save name)
        (begin
          (buffer-pretend-modified name)
          (set-message `(concat "Could not save " ,vname) "Save file"))
        (begin
          (if (== (url-suffix name) "ts") (style-clear-cache))
          (autosave-remove name)
          (buffer-notify-recent name)
          (set-message `(concat "Saved " ,vname) "Save file")
          (save-buffer-post name opts)))))

(define (save-buffer-check-faithful name opts)
  ;;(display* "save-buffer-check-faithful " name "\n")
  (if (has-faithful-format? name)
      (save-buffer-save name opts)
      (user-confirm "Save requires data conversion. Really proceed?" #f
        (lambda (answ)
          (when answ
            (save-buffer-save name opts))))))

(define (cannot-write? name action)
  (with vname `(verbatim ,(url->system name))
    (cond ((and (not (url-test? name "f")) (not (url-test? name "c")))
           (with msg `(concat "The file " ,vname " cannot be created")
             (set-message msg action))
           #t)
          ((and (url-test? name "f") (not (url-test? name "w")))
           (with msg `(concat "You do not have write access for " ,vname)
             (set-message msg action))
           #t)
          (else #f))))

(define (save-buffer-check-permissions name opts)
  ;;(display* "save-buffer-check-permissions " name "\n")
  (set! current-save-target name)
  (with vname `(verbatim ,(url->system name))
    (cond ((url-scratch? name)
           (choose-file
             (lambda (x) (apply save-buffer-as-main (cons x opts)))
             "Save TeXmacs file" ""))
          ((not (buffer-exists? name))
           (with msg `(concat "The buffer " ,vname " does not exist")
             (set-message msg "Save file")))
          ((not (buffer-modified? name))
           (with msg "No changes need to be saved"
             (set-message msg "Save file"))
           (save-buffer-post name opts))
          ((cannot-write? name "Save file")
           (noop))
          ((and (url-test? name "fr")
                (> (url-last-modified name)
                   (buffer-last-save name)))
           (user-confirm "The file has changed on disk. Really save?" #f
             (lambda (answ)
               (when answ
                 (save-buffer-check-faithful name opts)))))
          (else (save-buffer-check-faithful name opts)))))

(tm-define (save-buffer-main . args)
  ;;(display* "save-buffer-main\n")
  (if (or (null? args) (not (url? (car args))))
      (save-buffer-check-permissions (current-buffer) args)
      (save-buffer-check-permissions (car args) (cdr args))))

(tm-define (save-buffer . l)
  (apply save-buffer-main l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving buffers under a new name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-buffer-as-save new-name name opts)
  ;;(display* "save-buffer-as-save " new-name ", " name "\n")
  (if (and (url-scratch? name) (url-exists? name)) (system-remove name))
  (buffer-rename name new-name)
  (buffer-pretend-modified new-name)
  (save-buffer-save new-name opts))

(define (save-buffer-as-check-faithful new-name name opts)
  ;;(display* "save-check-as-check-faithful " new-name ", " name "\n")
  (if (or (== (url-suffix new-name) (url-suffix name))
          (has-faithful-format? new-name))
      (save-buffer-as-save new-name name opts)
      (user-confirm "Save requires data conversion. Really proceed?" #f
        (lambda (answ)
          (when answ
            (save-buffer-as-save new-name name opts))))))

(define (save-buffer-as-check-other new-name name opts)
  ;;(display* "save-buffer-as-check-other " new-name ", " name "\n")
  (cond ((buffer-exists? new-name)
         (with s (string-append "The file " (url->system new-name)
                                " is being edited. Discard edits?")
           (user-confirm s #f
             (lambda (answ)
               (when answ (save-buffer-as-save new-name name opts))))))
        (else (save-buffer-as-save new-name name opts))))

(define (save-buffer-as-check-permissions new-name name opts)
  ;;(display* "save-buffer-as-check-permissions " new-name ", " name "\n")
  (cond ((cannot-write? new-name "Save file")
         (noop))
        ((and (url-test? new-name "f") (nin? :overwrite opts))
         (user-confirm "File already exists. Really overwrite?" #f
           (lambda (answ)
             (when answ (save-buffer-as-check-other new-name name opts)))))
        (else (save-buffer-as-check-other new-name name opts))))

(tm-define (save-buffer-as-main new-name . args)
  ;;(display* "save-buffer-as-main " new-name "\n")
  (if (or (null? args) (not (url? (car args))))
      (save-buffer-as-check-permissions new-name (current-buffer) args)
      (save-buffer-as-check-permissions new-name (car args) (cdr args))))

(tm-define (save-buffer-as new-name . args)
  (:argument new-name texmacs-file "Save as")
  (:default  new-name (propose-name-buffer))
  (with opts (if (x-gui?) args (cons :overwrite args))
    (apply save-buffer-as-main (cons new-name opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (export-buffer-export name to fm opts)
  ;;(display* "export-buffer-export " name ", " to ", " fm "\n")
  (with vto `(verbatim ,(url->system to))
    (if (buffer-export name to fm)
        (set-message `(concat "Could not save " ,vto) "Export file")
        (set-message `(concat "Exported to " ,vto) "Export file"))))

(define (export-buffer-check-permissions name to fm opts)
  ;;(display* "export-buffer-check-permissions " name ", " to ", " fm "\n")
  (cond ((cannot-write? to "Export file")
         (noop))
        ((and (url-test? to "f") (nin? :overwrite opts))
         (user-confirm "File already exists. Really overwrite?" #f
           (lambda (answ)
             (when answ (export-buffer-export name to fm opts)))))
        (else (export-buffer-export name to fm opts))))

(tm-define (export-buffer-main name to fm opts)
  ;;(display* "export-buffer-main " name ", " to ", " fm "\n")
  (if (string? to) (set! to (url-relative (buffer-get-master name) to)))
  (if (url? to) (set! current-save-target to))
  (export-buffer-check-permissions name to fm opts))

(tm-define (export-buffer to)
  (with fm (url-format to)
    (if (== fm "generic") (set! fm "verbatim"))
    (export-buffer-main (current-buffer) to fm (list :overwrite))))

(tm-define (buffer-exporter fm)
  (with opts (if (x-gui?) (list) (list :overwrite))
    (lambda (s) (export-buffer-main (current-buffer) s fm opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (more-recent file suffix1 suffix2)
  (and (url-exists? (url-glue file suffix1))
       (url-exists? (url-glue file suffix2))
       (url-newer? (url-glue file suffix1) (url-glue file suffix2))))

(define (most-recent-suffix file)
  (if (more-recent file "~" "")
      (if (not (more-recent file "#" "")) "~"
          (if (more-recent file "#" "~") "#" "~"))
      (if (more-recent file "#" "") "#" "")))

(define (autosave-eligible? name)
  (and (not (url-rooted-web? name))
       (not (url-rooted-tmfs? name))))

(define (autosave-propose name)
  (and (autosave-eligible? name)
       (with s (most-recent-suffix name)
         (and (!= s "")
              (url-glue name s)))))

(define (autosave-rescue? name) 
  (and (autosave-eligible? name)
       (== (most-recent-suffix name) "#")))

(define (autosave-remove name)
  (when (file-exists? (url-glue name "~"))
    (system-remove (url-glue name "~")))
  (when (file-exists? (url-glue name "#"))
    (system-remove (url-glue name "#"))))

(tm-define (autosave-buffer name)
  (when (and (buffer-modified-since-autosave? name)
             (or (url-scratch? name)
                 (url-test? name "fw")
                 (and (not (url-exists? name))
                      (url-test? name "c"))))
    ;;(display* "Autosave " name "\n")
    ;; FIXME: incorrectly autosaves after cursor movements only
    (let* ((vname `(verbatim ,(url->system name)))
           (suffix (if (rescue-mode?) "#" "~"))
           (aname (url-glue name suffix))
           (fm (url-format name)))
      (if (url-scratch? name) (set! aname name))
      (cond ((!= fm "texmacs")
             (when (not (rescue-mode?))
               (set-message `(concat "Warning: " ,vname " not auto-saved")
                            "Auto-save file")))
            ((buffer-export name aname "texmacs")
             (when (not (rescue-mode?))
               (set-message `(concat "Failed to auto-save " ,vname)
                            "Auto-save file")))
            (else
             (when (not (rescue-mode?))
               (buffer-pretend-autosaved name)
               (set-temporary-message `(concat "Auto-saved " ,vname)
                                      "Auto-save file" 2500)))))))

(tm-define (autosave-all)
  (for-each autosave-buffer (buffer-list)))

(tm-define (autosave-now)
  (autosave-all)
  (autosave-delayed))

(tm-define (autosave-delayed)
  (let* ((pref (get-preference "autosave"))
	 (len (if (and (string? pref) (integer? (string->number pref)))
		  (* (string->number pref) 1000) 120000)))
    (if (> len 0)
	(delayed
	  (:pause len)
	  (autosave-now)))))

(define (notify-autosave var val)
  (if (current-view) ; delayed-autosave would crash at initialization time
      (autosave-delayed)))

(define-preferences
  ("autosave" "120" notify-autosave))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-buffer-open name opts)
  ;;(display* "load-buffer-open " name ", " opts "\n")
  (cond ((in? :background opts) (noop))
        ((in? :new-window opts)
         (open-buffer-in-window name (buffer-get name) ""))
        (else
          (switch-to-buffer name)))
  (buffer-notify-recent name)
  (noop))

(define (load-buffer-load name opts)
  ;;(display* "load-buffer-load " name ", " opts "\n")
  (with vname `(verbatim ,(url->system name))
    (if (url-exists? name)
        (if (buffer-load name)
            (set-message `(concat "Could not load " ,vname) "Load file")
            (load-buffer-open name opts))
        (begin
          (buffer-set-body name '(document ""))
          (load-buffer-open name opts)
          (set-message `(concat "Could not load " ,vname
                                ". Created new document")
                       "Load file")))))

(define (load-buffer-check-permissions name opts)
  ;;(display* "load-buffer-check-permissions " name ", " opts "\n")
  (with vname `(verbatim ,(url->system name))
    (cond ((and (not (url-test? name "f")) (not (url-test? name "c")))
           (with msg `(concat "The file " ,vname
                              " cannot be loaded or created")
             (set-message msg "Load file")))
          ((and (url-test? name "f") (not (url-test? name "r")))
           (with msg `(concat "You do not have read access to " ,vname)
             (set-message msg "Load file")))
          (else (load-buffer-load name opts)))))

(define (load-buffer-check-autosave name opts)
  ;;(display* "load-buffer-check-autosave " name ", " opts "\n")
  (if (and (autosave-propose name) (nin? :strict opts))
      (with question (if (autosave-rescue? name)
                         "Rescue file from crash?"
                         "Load more recent autosave file?")
        (user-confirm question #t
          (lambda (answ)
            (if answ
                (let* ((autosave-name (autosave-propose name))
                       (format (url-format name))
                       (doc (tree-import autosave-name format)))
                  (buffer-set name doc)
                  (load-buffer-open name opts)
                  (buffer-pretend-modified name))
                (load-buffer-check-permissions name opts)))))
      (load-buffer-check-permissions name opts)))

(tm-define (load-buffer-main name . opts)
  ;;(display* "load-buffer-main " name ", " opts "\n")
  (if (and (not (url-exists? name))
           (url-exists? (url-append "$TEXMACS_FILE_PATH" name)))
      (set! name (url-resolve (url-append "$TEXMACS_FILE_PATH" name) "f")))
  (if (not (url-rooted? name))
      (if (current-buffer)
          (set! name (url-relative (current-buffer) name))
          (set! name (url-append (url-pwd) name))))
  (load-buffer-check-autosave name opts))

(tm-define (load-buffer name . opts)
  (:argument name smart-file "File name")
  (:default  name (propose-name-buffer))
  ;;(display* "load-buffer " name ", " opts "\n")
  (apply load-buffer-main (cons name opts)))

(tm-define (load-buffer-in-new-window name . opts)
  (:argument name smart-file "File name")
  (:default  name (propose-name-buffer))
  (apply load-buffer-main (cons name (cons :new-window opts))))

(tm-define (load-browse-buffer name)
  (load-buffer name))

(tm-define (open-buffer)
  (:synopsis "Open a new file")
  (choose-file load-buffer "Load file" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reverting buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (revert-buffer . l)
  (with name (if (null? l) (current-buffer) (car l))
    (if (not (buffer-exists? name))
        (load-buffer name)
        (begin
          (when (!= name (current-buffer))
            (switch-to-buffer name))
          (url-cache-invalidate name)
          (with t (tree-import name (url-format name))
            (if (== t (tm->tree "error"))
                (set-message "Error: file not found" "Revert buffer")
                (buffer-set name t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (import-buffer-import name fm opts)
  ;;(display* "import-buffer-import " name ", " fm "\n")
  (if (== fm (url-format name))
      (load-buffer-main name opts)
      (let* ((s (url->tmfs-string name))
             (u (string-append "tmfs://import/" fm "/" s)))
        (load-buffer-main u opts))))

(define (import-buffer-check-permissions name fm opts)
  ;;(display* "import-buffer-check-permissions " name ", " fm "\n")
  (with vname `(verbatim ,(url->system name))
    (cond ((not (url-test? name "f"))
           (with msg `(concat "The file " ,vname " does not exist")
             (set-message msg "Import file")))
          ((not (url-test? name "r"))
           (with msg `(concat "You do not have read access to " ,vname)
             (set-message msg "Import file")))
          (else (import-buffer-import name fm opts)))))

(tm-define (import-buffer-main name fm opts)
  ;;(display* "import-buffer-main " name ", " fm "\n")
  (if (and (not (url-exists? name))
           (url-exists? (url-append "$TEXMACS_FILE_PATH" name)))
      (set! name (url-resolve (url-append "$TEXMACS_FILE_PATH" name) "f")))
  (import-buffer-check-permissions name fm opts))

(tm-define (import-buffer name fm . opts)
  (import-buffer-main name fm opts))

(tm-define (buffer-importer fm)
  (lambda (s) (import-buffer s fm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (print-buffer)
  (:synopsis "Print the current buffer")
  (print))

(tm-define (interactive-page-setup)
  (:synopsis "Specify the page setup")
  (:interactive #t)
  (set-message "Not yet implemented" "Printer setup"))

(tm-define (interactive-print-buffer)
  (:synopsis "Print the current buffer")
  (:interactive #t)
  (print-to-file "$TEXMACS_HOME_PATH/system/tmp/tmpprint.ps")
  (interactive-print '() "$TEXMACS_HOME_PATH/system/tmp/tmpprint.ps"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (set-abbr-buffer name abbr)
  (deprecated-function "set-abbr-buffer" "buffer-set-title")
  (buffer-set-title (current-buffer) abbr))

(tm-define (get-abbr-buffer name)
  (deprecated-function "get-abbr-buffer" "buffer-get-title")
  (buffer-get-title (current-buffer)))

(tm-define (set-buffer name doc)
  (deprecated-function "set-buffer" "buffer-set")
  (buffer-set name doc))

(tm-define (set-buffer-tree name doc)
  (deprecated-function "set-buffer-tree" "buffer-set")
  (set-buffer-tree name doc))

(tm-define (get-buffer-tree name)
  (deprecated-function "get-buffer-tree" "buffer-get-body")
  (get-buffer-tree name))

(tm-define (get-name-buffer-path p)
  (deprecated-function "get-name-buffer-path" "path->buffer")
  (path->buffer p))

(tm-define (get-name-buffer)
  (deprecated-function "get-name-buffer" "current-buffer")
  (current-buffer))

(tm-define (set-name-buffer name)
  (deprecated-function "set-name-buffer" "buffer-rename")
  (buffer-rename (current-buffer) name))

(tm-define (pretend-save-buffer)
  (deprecated-function "pretend-save-buffer" "buffer-pretend-saved")
  (buffer-pretend-saved (current-buffer)))

(tm-define (buffer-unsaved?)
  (deprecated-function "buffer-unsaved?" "buffer-modified?")
  (buffer-modified? (current-buffer)))

(tm-define (no-name?)
  (deprecated-function "no-name?" "buffer-has-name?")
  (not (buffer-has-name? (current-buffer))))

(tm-define (kill-buffer)
  (deprecated-function "kill-buffer" "buffer-close")
  (buffer-close (current-buffer)))
